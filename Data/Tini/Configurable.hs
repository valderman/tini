{-# OPTIONS_GHC -Wno-duplicate-exports #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, DataKinds, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, UndecidableInstances, UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Scaffolding for building, reading, updating and serializing configuration
--   data types. To create an INI-configurable data type:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > import Data.Tini.Configurable
-- >
-- > data MyConfig = MyConfig { someValue :: Int, ... }
-- >   deriving Generic
-- >
-- > instance Configurable MyConfig where
-- >   defaultConfig = MyConfig { someValue = 42, ... }
--
--   For more advanced usage, see <https://github.com/valderman/tini>.
module Data.Tini.Configurable
  ( -- * Basic configuration
    Configurable (defaultConfig)
  , fromIni, toIni
    -- * Working with config files
  , readConfigFile, readConfigFileWith, writeConfigFile, updateConfigFile
    -- * Advanced configuration
  , Configurable (..)
  , ConfigInvariants
    -- * Re-exports
  , Generic
  ) where
import Control.Exception (SomeException (..), try)
import Control.Monad ((>=>))
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import GHC.Generics
import GHC.TypeLits
import System.IO
import Data.Tini
import Data.Tini.Types

-- | Read the given INI file into a configuration, using 'defaultConfig' to
--   fill in any values missing from the config file.
readConfigFile :: Configurable a => FilePath -> IO a
readConfigFile = readConfigFileWith defaultConfig

-- | Update the given configuration with the settings read from the given
--   INI file.
readConfigFileWith :: Configurable a => a -> FilePath -> IO a
readConfigFileWith c = fmap (flip updateConfig c . maybe empty id) . readIniFile

-- | Update the given configuration file with the given config.
--
--   This function first reads the given file as an INI, overwrites
--   all its values with values from from the given config,
--   then writes the result back to disk atomically.
--   This ensures that no data is lost in the event of a crash, and that
--   the resulting file preserves both comments and keys
--   which are not recognized by the config are not clobbered.
updateConfigFile :: Configurable a => FilePath -> a -> IO ()
updateConfigFile f cfg = do
  old <- try $ withFile f ReadMode $ hGetContents >=> (pure $!) . parseIni
  case old :: Either SomeException (Maybe Ini) of
    Right (Just ini) -> writeIniFile f (updateIni cfg ini)
    _                -> writeIniFile f (toIni cfg)

-- | Write the given configuration to the given file.
--   If the file already exists, it will be overwritten.
writeConfigFile :: Configurable a => FilePath -> a -> IO ()
writeConfigFile file = writeIniFile file . toIni

-- | Create a configuration from the given INI.
--   Missing values will be filled in from 'defaultConfig'.
fromIni :: Configurable a => Ini -> a
fromIni = flip updateConfig defaultConfig

-- | Create a new INI configuration from the given config.
toIni :: Configurable a => a -> Ini
toIni = flip updateIni empty

-- | Invariants on the 'ExcludedFields' and 'IniSection' types of
--   'Configurable': all names appearing in @ExcludedFields@ must be names
--   of fields of @a@, @IniSection@ must be a proper string.
type ConfigInvariants a =
  ( FieldsOf a (ExcludedFields a)
  , SymList (ExcludedFields a)
  , KnownSymbol (IniSection a)
  )

-- | Any type which can be stored and loaded as an INI configuration file.
--
--   Using the default implementation, this includes all record types with a
--   single data constructor, and where all fields implement 'IniValue'.
class ConfigInvariants a => Configurable a where
  -- | Update the given configuration using settings from the given INI.
  --
  --   In the default implementation, settings are looked up by record selector.
  --   A record called @playerName@ will be overwritten with the value at key
  --   @playerName@ in the given INI, if such a key is present and
  --   of the correct type.
  updateConfig :: Ini -> a -> a
  default updateConfig :: (Generic a, GConfigurable (Rep a)) => Ini -> a -> a
  updateConfig ini =
    to . gUpdate (symList @(ExcludedFields a)) (section @a) Nothing ini . from

  -- | Update the given INI with the values from the given config.
  --   Implementations should preserve comments in the given INI (i.e. only
  --   update it using 'set').
  updateIni :: a -> Ini -> Ini
  default updateIni :: (Generic a, GConfigurable (Rep a)) => a -> Ini -> Ini
  updateIni cfg = gUpdIni (symList @(ExcludedFields a)) (section @a) Nothing (from cfg)

  -- | Fields of the configuration type which should not be
  --   read from or written to an INI.
  --   The default is to allow configuration of all fields.
  type ExcludedFields a :: [Symbol]
  type ExcludedFields a = '[]

  -- | The section of an INI under which to place settings from
  --   this configuration. The default is no section.
  type IniSection a :: Symbol
  type IniSection a = ""

  -- | The default values for this configuration.
  defaultConfig :: a
  {-# MINIMAL defaultConfig #-}

type family Member (x :: Symbol) (f :: * -> *) :: Nat where
  Member x (M1 S ('MetaSel ('Just x) u s d) f) = 1
  Member x (M1 i c f)                          = Member x f
  Member x (a :*: b)                           = Member x a + Member x b
  Member x a                                   = 0

type family FailUnless (p :: Nat) (f :: Symbol) (a :: *) :: Constraint where
  FailUnless 1 f a = ()
  FailUnless n f a = TypeError
    ( 'Text "Type '" ':<>: 'ShowType a ':<>: 'Text "' has no field named "
      ':<>: 'ShowType f ':<>: 'Text ", in declaration of 'ExcludedFields'."
    )

type family FieldsOf a (xs :: [Symbol]) :: Constraint where
  FieldsOf a (f ': fs) = (FailUnless (Member f (Rep a)) f a, FieldsOf a fs)
  FieldsOf a '[]       = ()

class GConfigurable f where
  gUpdate :: [String] -> SectionHead -> Maybe Key -> Ini -> f a -> f a
  gUpdIni :: [String] -> SectionHead -> Maybe Key -> f a -> Ini -> Ini

section :: forall a. KnownSymbol (IniSection a) => SectionHead
section = SH (symbolVal (Proxy :: Proxy (IniSection a)))

class SymList (a :: [Symbol]) where
  symList :: [String]

instance (KnownSymbol x, SymList xs) => SymList (x ': xs) where
  symList = symbolVal (Proxy @x) : symList @xs
instance SymList '[] where
  symList = []

instance (GConfigurable f, Selector c) => GConfigurable (M1 S c f) where
  gUpdate exclude s _ ini (M1 m)
    | sel `elem` exclude = M1 $ gUpdate exclude s Nothing ini m
    | otherwise          = M1 $ gUpdate exclude s (Just (Key s sel)) ini m
    where sel = selName (undefined :: M1 s c f a)
  gUpdIni exclude s _ (M1 m)
    | sel `elem` exclude = gUpdIni exclude s Nothing m
    | otherwise          = gUpdIni exclude s (Just (Key s sel)) m
    where sel = selName (undefined :: M1 s c f a)

instance {-# OVERLAPPABLE #-} GConfigurable f => GConfigurable (M1 i c f) where
  gUpdate e s key ini (M1 m) = M1 $ gUpdate e s key ini m
  gUpdIni e s k (M1 m) = gUpdIni e s k m

instance IniValue a => GConfigurable (K1 i a) where
  gUpdate _ _ (Just key) ini k@(K1 _) = maybe k K1 (get ini key)
  gUpdate _ _ _ _ k                   = k

  gUpdIni _ _ (Just key) (K1 x) = set key x
  gUpdIni _ _ _ _               = id

instance (GConfigurable a, GConfigurable b) => GConfigurable (a :*: b) where
  gUpdate e s _ ini (a:*:b) = gUpdate e s Nothing ini a :*: gUpdate e s Nothing ini b
  gUpdIni e s _ (a:*:b) = gUpdIni e s Nothing b . gUpdIni e s Nothing a