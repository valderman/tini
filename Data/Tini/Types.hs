{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Data.Tini.Types where
import Data.String
import Data.Tini.Utils (rtrim)

-- | An ordered INI file.
newtype Ini = Ini { unIni :: [Section] }

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
--
--   A valid key is either of the format @"sect.prop"@ or just @"prop"@.
--   The first key will match the property @prop@ in section @sect@,
--   and the second will match the property @prop@ outside of any section.
--   @prop@ must not begin with @;@ or @#@ or contain a @=@,
--   and @sect@ must not contain @]@.
data Key = Key !SectionHead !String
  deriving (Eq, Ord)
instance IsString Key where
  fromString key =
    case break (== '.') key of
      (k, "")  -> Key "" (valid k)
      (s, _:k) -> Key (fromString s) (valid k)
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

newtype SectionHead = SH String
  deriving (Eq, Ord, IsString)
instance Show SectionHead where
  show (SH h) | ']' `elem` h = error "section head must not contain ']'"
              | otherwise    = h

type Section = (SectionHead, [Property])
type Property = (KeyOrComment, String)