module Data.Tini.Ops
  ( empty, get, set, remove, modify
  , toList, fromList, merge
  ) where
import Data.List (foldl')
import Data.Tini.IniValue
import Data.Tini.Types

-- | An INI with no sections or properties.
empty :: Ini
empty = Ini []

-- | Returns the value at the given key, if it exists and is valid at
--   the function's result type.
--   See 'IniValue' for more information regarding how Haskell values are
--   encoded in INI files.
get :: IniValue a => Ini -> Key -> Maybe a
get (Ini ini) (Key s k) = lookup s ini >>= lookup (KeyPart k) >>= readValue

-- | Sets the given key to the given value.
--   If the key already exists, it will be overwritten.
--
--   New sections are added at the end of the given INI, and new properties
--   are added at the end of their respective sections.
set :: IniValue a => Ini -> Key -> a -> Ini
set ini key val = modify (const (Just (showValue val))) key ini

-- | Removes the given key from the given INI.
remove :: Key -> Ini -> Ini
remove = modify (const Nothing)

upd :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
upd f k xs =
  case break ((== k) . fst) xs of
    (pre, (_, v):suf) -> pre ++ maybe [] (\v' -> [(k, v')]) (f (Just v)) ++ suf
    (_, [])           -> xs  ++ maybe [] (\v -> [(k, v)])   (f Nothing)

-- | Modify the value at the given key in the given INI.
--
--   If the key exists, the given function will receive @Just value@ as its
--   argument, otherwise it will receive @Nothing@.
--
--   If the given function returns @Just new_value@, the given key will be
--   created or overwritten with @new_value@. If it returns @Nothing@, the
--   key will be deleted.
modify :: (Maybe String -> Maybe String) -> Key -> Ini -> Ini
modify f (Key s k) = Ini . upd (Just . upd f (KeyPart k) . maybe [] id) s . unIni

-- | Convert the given INI to s list of @(key, value)@ pairs.
toList :: Ini -> [(Key, String)]
toList (Ini secs) = [(Key sec k, v) | (sec, ps) <- secs, (KeyPart k, v) <- ps]

-- | Create an INI from the given list of @(key, value)@ pairs.
fromList :: [(Key, String)] -> Ini
fromList = foldl' (uncurry . set) empty

-- | Merge the given INIs. Values from the second INI override values from
--   the first in cases where a key exists in both.
--   Comments from the second INI are discarded.
merge :: Ini -> Ini -> Ini
merge l = foldl' (uncurry . set) l . toList