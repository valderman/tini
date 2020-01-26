{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Data.Tini.Types where
import Data.Char (isSpace)
import Data.String

-- | An ordered INI file.
newtype Ini = Ini { unIni :: [Section] }

rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse

instance Show Ini where
  show (Ini ini) = rtrim $ concat
      [ maybe "" ((<> "\n") . unlines . map showProp) (lookup "" ini)
      , concat (map showSection ini)
      ]
    where
      showSection ("", _) = ""
      showSection (s, ps) = "[" <> show s <> "]\n" <> unlines (map showProp ps) <> "\n"
      showProp (KeyPart key, value) = key <> " = " <> value
      showProp (Comment c, comment) = c <> comment

-- | A key into an INI file.
data Key = Key SectionHead String
  deriving (Eq, Ord)
instance IsString Key where
  fromString key =
    case break (== '.') key of
      (k, "")  -> Key "" (valid k)
      (s, _:k) -> Key (fromString s) (valid k)
instance Read Key where
  readsPrec _ s = [(fromString s, "")]
instance Show Key where
  show (Key "" key)   = key
  show (Key sect key) = show sect <> "." <> key

valid :: String -> String
valid (';':_)              = error "keys must not start with ';'"
valid ('#':_)              = error "keys must not start with '#'"
valid key | '=' `elem` key = error "keys must not contain '='"
          | otherwise      = key


data KeyOrComment = Comment !String | KeyPart !String
instance Ord KeyOrComment where
  compare (KeyPart a) (KeyPart b) = compare a b
  compare _           _           = LT
instance Eq KeyOrComment where
  KeyPart a == KeyPart b = a == b
  _         == _         = False
instance Show KeyOrComment where
  show (Comment c) = c
  show (KeyPart k) = k

newtype SectionHead = SectionHead String
  deriving (Eq, Ord, IsString)
instance Show SectionHead where
  show (SectionHead h) = h

type Section = (SectionHead, [Property])
type Property = (KeyOrComment, Value)
type Value = String
