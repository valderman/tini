{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, CPP #-}
module Data.Tini.Types where
import Data.Char (isSpace)
import Data.List
#if !MIN_VERSION_base(4, 11, 0)
import Data.Monoid
#endif
import Data.String
import Data.Tini.Utils (ltrim, rtrim, trim)

-- | An ordered, comment-preserving representation of an INI file.
newtype Ini = Ini { unIni :: [Section] }
  deriving (Show, Read)

-- | Serialize an @Ini@ to a @String@.
showIni :: Ini -> String
showIni (Ini ini) = rtrim $ concat
    [ maybe "" ((<> "\n") . unlines . map showProp) (lookup "" ini)
    , concat (map showSection ini)
    ]
  where
    showSection ("", _) = ""
    showSection (s, ps) = "[" <> showSN s <> "]\n" <> unlines (map showProp ps) <> "\n"
    showProp (KeyPart key, value) = key <> " = " <> value
    showProp (Comment c, comment) = c <> comment

instance Eq Ini where
  Ini a == Ini b =
    sort [(sn, sort p) | (sn, p) <- a] == sort [(sn, sort p) | (sn, p) <- b]

-- | A key into an INI file.
--
--   A valid key is either of the format @"sect.prop"@ or just @"prop"@.
--   The first key will match the property @prop@ in section @sect@,
--   and the second will match the property @prop@ outside of any section.
--   @prop@ must not begin with @;@ or @#@ or contain a @=@,
--   and @sect@ must not contain @]@. Both parts are case-sensitive.
data Key = Key !SectionName !String
  deriving (Eq, Ord)
instance IsString Key where
  fromString key =
    case break (== '.') (trim key) of
      (k, "")  -> Key "" (valid k)
      (s, _:k) -> Key (fromString (rtrim s)) (valid k)

instance Show Key where
  show (Key "" key)   = key
  show (Key sect key) = show sect <> "." <> key

valid :: String -> String
valid ('[':_)               = error "keys must not start with '['"
valid (';':_)               = error "keys must not start with ';'"
valid ('#':_)               = error "keys must not start with '#'"
valid key | '=' `elem` key  = error "keys must not contain '='"
          | '\n' `elem` key = error "keys must not contain newlines"
          | all isSpace key = error $ "keys must not be empty/all whitespace"
          | otherwise       = ltrim key

data KeyOrComment = Comment !String | KeyPart !String
  deriving (Show, Read)
instance Ord KeyOrComment where
  compare (KeyPart a) (KeyPart b) = compare a b
  compare _           _           = LT
instance Eq KeyOrComment where
  KeyPart a == KeyPart b = a == b
  Comment a == Comment b = a == b
  _         == _         = False

-- | The name of an INI section.
--   Must not contain the character @']'@.
newtype SectionName = SN String
  deriving (Show, Read, Eq, Ord, IsString)
showSN :: SectionName -> String
showSN (SN h) | ']' `elem` h = error "section head must not contain ']'"
                       | otherwise    = h

type Section = (SectionName, [Property])
type Property = (KeyOrComment, String)