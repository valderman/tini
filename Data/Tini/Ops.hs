{-# LANGUAGE GADTs, TypeApplications, ScopedTypeVariables #-}
module Data.Tini.Ops
  ( empty, get, getAs, set, remove, modify
  , toList, fromList, merge
  ) where
import Data.Char (toLower)
import Data.List (foldl')
import Data.Typeable
import Data.Tini.Types

-- | An INI with no sections or properties.
empty :: Ini
empty = Ini []

-- | Returns the value at the given key, if it exists and can be parsed as
--   the function's result type.
--   All values are valid strings, and the following values are
--   valid booleans: @true, yes, t, y, 1, on, false, no, f, n, 0, off@.
--
--   Using @TypeApplications@ to explicitly specify the return type
--   is usually a good idea:
--
-- > getAs @Int ini "person.age"
getAs :: (Typeable a, Read a) => Ini -> Key -> Maybe a
getAs ini key = get ini key >>= readMaybe

readMaybe :: forall a. (Typeable a, Read a) => String -> Maybe a
readMaybe s =
    case eqT @a @String of
      Just Refl -> Just s
      _         -> case eqT @a @Bool of
        Just Refl -> readBool (map toLower s)
        _         -> tryRead s
  where
    readBool s
      | s `elem` ["true",  "yes", "t", "y", "1", "on"]  = Just True
      | s `elem` ["false", "no",  "f", "n", "0", "off"] = Just False
      | otherwise                                       = Nothing
    tryRead s =
      case reads s of
        [(x, "")] -> Just x
        _         -> Nothing

-- | Returns the value at hte given key, if it exists.
--
--   A valid key is either of the format @sect.prop@ or just @prop@.
--   The first key will match the property @prop@ in section @sect@,
--   and the second will match the property @prop@ outside of any section.
--   The @prop@ part must not begin with @;@ or @#@, or contain a @=@.
get :: Ini -> Key -> Maybe String
get (Ini ini) (Key s k) = lookup s ini >>= lookup (KeyPart k)

-- | Sets the given key to the given value.
--   If the key already exists, it will be overwritten.
--
--   New sections are added at the end of the given INI, and new properties
--   are added at the end of their respective sections.
set :: (Typeable a, Show a) => Key -> a -> Ini -> Ini
set key val = modify (const (Just (show' val))) key

show' :: forall a. (Typeable a, Show a) => a -> String
show' x =
  case eqT @a @String of
    Just Refl -> x
    _         -> case eqT @a @Bool of
      Just Refl -> map toLower (show x)
      _         -> show x

-- | Removes the given key from the given INI.
remove :: Key -> Ini -> Ini
remove = modify (const Nothing)

upd :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
upd f k xs =
  case break ((== k) . fst) xs of
    (prefix, (_, v):suffix) ->
      case f (Just v) of
        Just v' -> prefix ++ (k, v') : suffix
        _       -> prefix ++ suffix
    (_, []) -> 
      case f Nothing of
        Just v -> xs ++ [(k, v)]
        _      -> xs

-- | Modify the value at the given key in the given INI.
--
--   If the key exists, the given function will receive @Just value@ as its
--   argument, otherwise it will receive @Nothing@.
--
--   If the given function returns @Just new_value@, the given key will be
--   created or overwritten with @new_value@. If it returns @Nothing@, the
--   key will be deleted.
modify :: (Maybe Value -> Maybe Value) -> Key -> Ini -> Ini
modify f (Key s k) = Ini . upd (Just . upd f (KeyPart k) . maybe [] id) s . unIni

-- | Convert the given INI to s list of @(key, value)@ pairs.
toList :: Ini -> [(Key, Value)]
toList (Ini sections) =
  [ (Key sect name, value)
  | (sect, props) <- sections
  , (KeyPart name, value) <- props
  ]

-- | Create an INI from the given list of @(key, value)@ pairs.
fromList :: [(Key, Value)] -> Ini
fromList = foldl' (flip $ uncurry set) empty

-- | Merge the given INIs. Values from the second INI override values from
--   the first in cases where a key exists in both.
--   Comments from the second INI are discarded.
merge :: Ini -> Ini -> Ini
merge l = foldl' (flip $ uncurry set) l . toList