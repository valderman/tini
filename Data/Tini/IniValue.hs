{-# LANGUAGE DefaultSignatures, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
module Data.Tini.IniValue where
import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Int
import Data.List (intercalate)
import Data.Word
import Data.Tini.Utils (trim, splitOn)

-- | Valid types for values of INI properties.
--   Default implementation uses 'show' and 'reads'
--   for @showValue@ and @readValue@ respectively.
class IniValue a where
  showValue :: a -> String
  default showValue :: Show a => a -> String
  showValue = show

  readValue :: String -> Maybe a
  default readValue :: Read a => String -> Maybe a
  readValue s =
    case reads s of
      [(x, "")] -> Just x
      _         -> Nothing

instance IniValue Int
instance IniValue Int8
instance IniValue Int16
instance IniValue Int32
instance IniValue Int64
instance IniValue Word
instance IniValue Word8
instance IniValue Word16
instance IniValue Word32
instance IniValue Word64
instance IniValue Integer
instance IniValue Float
instance IniValue Double

instance IniValue Char where
  showValue c = [c]
  readValue [c] = Just c
  readValue _   = Nothing

-- | Valid values for booleans (case-insensitive):
--   true, false, yes, no, t, f, y, n, 1, 0, on, off.
instance IniValue Bool where
  showValue True  = "true"
  showValue False = "false"
  readValue s
    | s' `elem` ["true",  "yes", "t", "y", "1", "on"]  = Just True
    | s' `elem` ["false", "no",  "f", "n", "0", "off"] = Just False
    | otherwise                                        = Nothing
      where s' = map toLower s

-- | Strings are the raw values of properties, with whitespace trimmed on both
--   ends. They are not enclosed in quotes.
instance {-# OVERLAPPING #-} IniValue String where
  showValue = id
  readValue = Just

-- | Lists are zero or more valid values, separated by commas.
--   To include a comma in a string within a list, escape it using @\\,@.
instance IniValue a => IniValue [a] where
  showValue = intercalate "," . map showValue
  readValue = sequence . map readValue . map trim . splitOn ','

-- | @Nothing@ is encoded as the empty string, and any non-empty value
--   of the correct type encodes @Just val@.
instance IniValue a => IniValue (Maybe a) where
  showValue = maybe "" showValue
  readValue "" = Just Nothing
  readValue x  = Just $ readValue x

-- | @readValue@ returns @Left val@ If the value is readable at the type
--   on the left, @Right val@ if it's readable at the type on the right,
--   and @Nothing@ if it's not readable as either.
instance (IniValue a, IniValue b) => IniValue (Either a b) where
  showValue = either showValue showValue
  readValue s = (Left <$> readValue s) <|> (Right <$> readValue s)

-- | Tuples follow the same rules as lists, but must be of the correct length.
instance (IniValue a, IniValue b) => IniValue (a, b) where
  showValue (a, b) = showValue [showValue a, showValue b]
  readValue s = do
    [a, b] <- readValue s
    (,) <$> readValue a <*> readValue b

instance (IniValue a, IniValue b, IniValue c) => IniValue (a, b, c) where
  showValue (a, b, c) = showValue [showValue a, showValue b, showValue c]
  readValue s = do
    Just [a, b, c] <- readValue s
    (,,) <$> readValue a <*> readValue b <*> readValue c