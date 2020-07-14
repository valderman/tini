module Data.Tini.Parser (parseIni) where
import Prelude hiding (fail)
import Control.Applicative (Alternative, (<|>))
import Control.Monad ((<=<))
import Control.Monad.Fail (MonadFail, fail)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (intercalate, sortBy, groupBy)
import Data.Tini.Types
import Data.Tini.Utils (trim, ltrim, rtrim)

-- | Parses an INI file from the given string.
--
--   A valid INI file may contain zero or more lines, where each line is any
--   of the following:
--
--   * A @[section header]@ in square brackets;
--   * a @key = value@ pair;
--   * a comment, starting with either @;@ or @#@; or
--   * whitespace.
--
--   Note that a valid INI file must not contain duplicate section headers,
--   and keys must be unique within their section.
--   Section headers and keys are case-sensitive.
--   Values must be contained on a single line.
--   Whitespace is ignored at the start and end of each line, section header,
--   key, and value.
parseIni :: (Alternative m, MonadFail m) => String -> m Ini
parseIni = fmap Ini . undupe "sections" <=< parseSections "" . filterWS . lines
  where filterWS = filter (not . null) . map ltrim

undupe :: (Show k, Ord k, Eq k, MonadFail m) => String -> [(k, v)] -> m [(k, v)]
undupe what ss =
    case map (fst . head) $ filter duplicate (dupeGroups ss) of
      [] -> return ss
      ds -> fail $ "duplicate " <> what <> ": " <> intercalate ", " (map show ds)
  where
    duplicate (_:_:_) = True
    duplicate _       = False
    dupeGroups = groupBy ((==) `on` fst) . sortBy (compare `on` fst)

parseSections :: (Alternative m, MonadFail m) => String -> [String] -> m [Section]
parseSections name lns =
  case break isSectionHead lns of
    ([], [])        -> return []
    (section, [])   -> do
      props <- parseProps section
      return [(SN name, props)]
    (section, s:ss) -> do
      name' <- parseSectionHead s
      props <- parseProps section
      sections <- parseSections name' ss
      if null section
        then return sections
        else return ((SN name, props):sections)

parseProps :: (Alternative m, MonadFail m) => [String] -> m [Property]
parseProps = undupe "keys" <=< mapM (\s -> parseComment s <|> parseProp s)

isSectionHead :: String -> Bool
isSectionHead ('[':_) = True
isSectionHead _       = False 

parseSectionHead :: MonadFail m => String -> m String
parseSectionHead ('[':s) =
  case break (== ']') s of
    (name, ']':suffix)
      | all isSpace suffix -> return name
      | otherwise          -> fail $ "section head '" <> name <> "' has trailing garbage"
    _                      -> fail $ "unclosed section head '" <> s <> "'"
parseSectionHead s =
  fail $ "expected section head, but got '" <> s <> "'"

parseProp :: MonadFail m => String -> m Property
parseProp s =
  case break (== '=') s of
    (k@(_:_), '=':v) -> return (KeyPart (trim k), trim v)
    _                -> fail $ "expected 'key = value', but got '" <> s <> "'"

parseComment :: MonadFail m => String -> m Property
parseComment (';':s) = return (Comment ";", s)
parseComment ('#':s) = return (Comment "#", s)
parseComment s       = fail $ "expected comment, but got '" <> s <> "'"