module Data.Tini.Utils where
import Data.Char (isSpace)

ltrim :: String -> String
ltrim = dropWhile isSpace

rtrim :: String -> String
rtrim = reverse . dropWhile isSpace . reverse

trim :: String -> String
trim = ltrim . rtrim

splitOn :: Char -> String -> [String]
splitOn _ ""  = []
splitOn c str = go "" str
  where
    go s ('\\':x:xs) | x == c    = go (c:s) xs
    go s (x:xs)      | x == c    = reverse s : go "" xs
                     | otherwise = go (x:s) xs
    go s []                      = [reverse s]