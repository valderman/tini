{-# LANGUAGE CPP, TypeApplications #-}
module Data.Tini
  ( IniValue (..), Ini, Key, SectionHead
  , parseIni
  , module Data.Tini.Ops
  , readIniFile, writeIniFile
  ) where
import Control.Exception (SomeException, try)
import System.IO
import Data.Tini.IniValue
import Data.Tini.Ops
import Data.Tini.Parser (parseIni)
import Data.Tini.Rename (rename)
import Data.Tini.Types (Ini, Key, SectionHead)

-- | Attempt to read the given file as an INI.
--   Returns @Nothing@ if the file does not exist or can not be parsed as
--   an INI file.
readIniFile :: FilePath -> IO (Maybe Ini)
readIniFile file = do
  ini <- try $ withFile file ReadMode $ \h -> do
    (pure $!) . parseIni =<< hGetContents h
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
#ifdef mingw32_HOST_OS
    dirSeparator = '\\'
#else
    dirSeparator = '/'
#endif
    splitPath p =
      case break (== dirSeparator) (reverse p) of
        (f, "")    -> (".", reverse f)
        (f, _:d) -> (reverse d, reverse f)