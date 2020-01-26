{-# LANGUAGE CPP, TypeApplications #-}
module Data.Tini
  ( IniValue (..), Ini, Key
  , parseIni, parseIni'
  , module Data.Tini.Ops
  , readIniFile, writeIniFile
  ) where
import Control.Exception (SomeException, try)
import System.IO
import Data.Tini.IniValue
import Data.Tini.Ops
import Data.Tini.Parser (parseIni)
import Data.Tini.Rename (rename)
import Data.Tini.Types (Ini, Key)

-- | Try to parse the given string as an INI file,
--   returning the empty INI on failure.
parseIni' :: String -> Ini
parseIni' = maybe empty id . parseIni

-- | Attempt to read the given file as an INI.
--   Returns @Nothing@ if the file does not exist or can not be parsed as
--   an INI file.
readIniFile :: FilePath -> IO (Maybe Ini)
readIniFile file = do
  ini <- try $ withFile file ReadMode $ \h -> do
    contents <- hGetContents h
    return $! parseIni contents
  case ini :: Either SomeException (Maybe Ini) of
    Right s -> return $! s
    _       -> return Nothing

-- | Atomically write the given INI to the given file.
writeIniFile :: FilePath -> Ini -> IO ()
writeIniFile path ini = do
    (tmpfile, h) <- openTempFile dir file
    hPutStrLn h (show ini) >> hFlush h >> hClose h
    rename tmpfile path
  where
    (dir, file) = splitPath path

dirSeparator :: Char
#ifdef mingw32_HOST_OS
dirSeparator = '\\'
#else
dirSeparator = '/'
#endif

splitPath :: FilePath -> (FilePath, FilePath)
splitPath path =
  case break (== dirSeparator) (reverse path) of
    (file, "")    -> (".", reverse file)
    (file, _:dir) -> (reverse dir, reverse file)