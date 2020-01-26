{-# LANGUAGE TypeOperators, ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
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
module Data.Tini.Configurable
  ( -- * Basic configuration
    Configurable (..)
  , configure, serialize
    -- * Working with config files
  , readConfigFile, readConfigFileWith, writeConfigFile, updateConfigFile
    -- * Re-exports
  , Generic
  ) where
import Control.Exception (SomeException (..), try)
import Control.Monad ((>=>))
import Data.Proxy (Proxy (..))
import GHC.Generics
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
    _                -> writeIniFile f (serialize cfg)

-- | Write the given configuration to the given file.
--   If the file already exists, it will be overwritten.
writeConfigFile :: Configurable a => FilePath -> a -> IO ()
writeConfigFile file = writeIniFile file . serialize

-- | Create a configuration from the given INI.
configure :: Configurable a => Ini -> a
configure = flip updateConfig defaultConfig

-- | Create a new INI configuration from the given config.
serialize :: Configurable a => a -> Ini
serialize = flip updateIni empty

-- | Any type which can be stored and loaded as an INI configuration file.
--
--   Using the default implementation, this includes all record types with a
--   single data constructor, and where all fields implement 'IniValue'.
class Configurable a where
  -- | Update the given configuration using settings from the given INI.
  --
  --   In the default implementation, settings are looked up by record selector.
  --   A record called @playerName@ will be overwritten with the value at key
  --   @playerName@ in the given INI, if such a key is present and
  --   of the correct type.
  updateConfig :: Ini -> a -> a
  default updateConfig :: (Generic a, GConfigurable (Rep a)) => Ini -> a -> a
  updateConfig ini = to . gUpdate (maybe "" id sec) Nothing ini . from
    where sec = configSection (Proxy :: Proxy a)

  -- | Update the given INI with the values from the given config.
  --   Implementations should preserve comments in the given INI (i.e. only
  --   update it using 'set').
  updateIni :: a -> Ini -> Ini
  default updateIni :: (Generic a, GConfigurable (Rep a)) => a -> Ini -> Ini
  updateIni cfg = gUpdIni (maybe "" id sec) Nothing (from cfg)
    where sec = configSection (Proxy :: Proxy a)

  -- | The section of an INI represented by this configuration.
  --
  --   If a given, all ini lookups made by the default implementation
  --   are performed within the namespace of this section.
  --   That is, if the section head is given as
  --   @Just "PlayerInfo"@, a record called @playerName@ will be overwritten
  --   with the value at key @PlayerInfo.playerName@ instead.
  configSection :: Proxy a -> Maybe SectionHead
  configSection _ = Nothing

  -- | The default values for this configuration.
  defaultConfig :: a
  {-# MINIMAL defaultConfig #-}

class GConfigurable f where
  gUpdate :: SectionHead -> Maybe Key -> Ini -> f a -> f a
  gUpdIni :: SectionHead -> Maybe Key -> f a -> Ini -> Ini

instance (GConfigurable f, Selector c) => GConfigurable (M1 S c f) where
  gUpdate s _ ini (M1 m) = M1 $ gUpdate s (Just (Key s sel)) ini m
    where sel = selName (undefined :: M1 s c f a)
  gUpdIni s _ (M1 m) = gUpdIni s (Just (Key s sel)) m
    where sel = selName (undefined :: M1 s c f a)

instance {-# OVERLAPPABLE #-} GConfigurable f => GConfigurable (M1 i c f) where
  gUpdate s key ini (M1 m) = M1 $ gUpdate s key ini m
  gUpdIni s k (M1 m) = gUpdIni s k m

instance IniValue a => GConfigurable (K1 i a) where
  gUpdate _ (Just key) ini k@(K1 _) = maybe k K1 (get ini key)
  gUpdate _ _ _ k                   = k

  gUpdIni _ (Just key) (K1 x) = set key x
  gUpdIni _ _ _               = id

instance (GConfigurable a, GConfigurable b) => GConfigurable (a :*: b) where
  gUpdate s _ ini (a:*:b) = gUpdate s Nothing ini a :*: gUpdate s Nothing ini b
  gUpdIni s _ (a:*:b) = gUpdIni s Nothing b . gUpdIni s Nothing a