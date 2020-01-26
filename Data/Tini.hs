{-# LANGUAGE TypeApplications #-}
module Data.Tini
  ( Ini, Key, Value
  , parseIni, parseIni'
  , module Data.Tini.Ops
  , readIniFile, writeIniFile
  ) where
import Control.Exception (SomeException, try)
import Data.Tini.Parser
import Data.Tini.Ops
import Data.Tini.Types

-- | Try to parse the given string as an INI file,
--   returning the empty INI on failure.
parseIni' :: String -> Ini
parseIni' = maybe empty id . parseIni

-- | Attempt to read the given file as an INI.
--   Returns @Nothing@ if the file does not exist or can not be parsed as
--   an INI file.
readIniFile :: FilePath -> IO (Maybe Ini)
readIniFile path = do
  content <- try @SomeException $ readFile path
  case content of
    Right s -> return (parseIni s)
    _       -> return Nothing

-- | Write the given INI to the given file.
writeIniFile :: FilePath -> Ini -> IO ()
writeIniFile path = writeFile path . show