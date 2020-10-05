{-# LANGUAGE TypeApplications, FlexibleInstances, OverloadedStrings, CPP #-}
module Main where
import Control.Monad (forM_, unless)
#if !MIN_VERSION_base(4, 13, 0)
import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail, fail)
#endif
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (deleteBy)
import System.Exit (exitFailure)
import Test.QuickCheck hiding (allProperties)
import Test.QuickCheck.Monadic
import Data.Tini
import Generators ()

instance MonadFail (Either String) where
  fail = Left

main :: IO ()
main = forM_ allProperties $ \prop -> do
  result <- quickCheckResult prop
  unless (isSuccess result) exitFailure

allProperties :: [Property]
allProperties =
  [ property equalityIsReflexive
  , property serializationAndDeserializationAreInverses
  , property removeIsIdempotent
  , property removeRemovesElement
  , property removeLeavesOtherElementsIntact
  , property setSetsElement
  , property setLeavesOtherElementsIntact
  , property commentsArePreserved
  , property modifyOverwritesElement
  , property modifyModifiesElement
  , property modifyLeavesOtherElementsIntact
  , property readAndWriteAreInverses
  ]

clean :: String -> String
clean = trim . filter (/= '\n')
  where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

equalityIsReflexive :: Ini -> Property
equalityIsReflexive ini =
  ini === ini

serializationAndDeserializationAreInverses :: Ini -> Property
serializationAndDeserializationAreInverses ini =
  Right @String ini === parseIni (showIni ini)

removeIsIdempotent :: Ini -> Key -> Property
removeIsIdempotent ini key =
  remove key ini === remove key (remove key ini)

removeRemovesElement :: Ini -> NonNegative Int -> Property
removeRemovesElement ini (NonNegative ix) =
    not (null prop_list) ==>
      get (remove key ini) key === Nothing @String
  where
    prop_list = toList ini
    key = fst $ prop_list !! (ix `rem` length prop_list)

removeLeavesOtherElementsIntact :: Ini -> NonNegative Int -> Property
removeLeavesOtherElementsIntact ini (NonNegative ix) =
    not (null prop_list) ==>
      prop_list_without_key === deleteBy ((==) `on` fst) pair prop_list
  where
    prop_list = toList ini
    pair@(key, _) = prop_list !! (ix `rem` length prop_list)
    prop_list_without_key = toList $ remove key ini

setSetsElement :: Ini -> Key -> String -> Property
setSetsElement ini key value =
  get (set ini key value) key === Just (clean value)

setLeavesOtherElementsIntact :: Ini -> Key -> String -> Property
setLeavesOtherElementsIntact ini key value =
    deleteBy ((==) `on` fst) (key, value) prop_list_with_key === prop_list
  where
    prop_list = toList ini
    prop_list_with_key = toList $ set ini key value

commentsArePreserved :: Ini -> String -> NonNegative Int -> Property
commentsArePreserved ini comment (NonNegative ix) =
    parseIni (showIni ini_with_comment) === Just ini_with_comment .&&.
    filter (not . null) (lines (showIni ini_with_comment)) === lns_with_comment
  where
    lns = filter (not . null) $ lines $ showIni ini
    comment_ix = if (null lns) then 0 else ix `rem` length lns
    comment_line = ';' : clean comment
    lns_with_comment = take comment_ix lns ++ comment_line : drop comment_ix lns
    Just ini_with_comment = parseIni (unlines lns_with_comment)

modifyOverwritesElement :: Ini -> NonNegative Int -> String -> Property
modifyOverwritesElement ini (NonNegative ix) value =
    get (modify (\_ -> Just value) key ini) key === Just (clean value)
  where
    prop_list = toList ini
    key = if null prop_list
            then "blah"
            else fst $ prop_list !! (ix `rem` length prop_list)

modifyModifiesElement :: Ini -> NonNegative Int -> Property
modifyModifiesElement ini (NonNegative ix) =
    if null prop_list
      then property (null (toList modified_ini))
      else get modified_ini key === fmap reverse (get @String ini key)
  where
    prop_list = toList ini
    key = if null prop_list
            then "blah"
            else fst $ prop_list !! (ix `rem` length prop_list)
    upd (Just value) = Just (reverse value)
    upd Nothing      = Nothing
    modified_ini = modify upd key ini

modifyLeavesOtherElementsIntact :: Ini -> NonNegative Int -> String -> Property
modifyLeavesOtherElementsIntact ini (NonNegative ix) value =
    remove key ini_with_modified_key === remove key ini
  where
    prop_list = toList ini
    key = if null prop_list
            then "blah"
            else fst $ prop_list !! (ix `rem` length prop_list)
    ini_with_modified_key = modify (\_ -> Just value) key ini

readAndWriteAreInverses :: Ini -> Property
readAndWriteAreInverses ini = monadicIO $ do
  run $ writeIniFile "tini-test.tmp" ini
  ini' <- run $ readIniFile "tini-test.tmp"
  assert (Just ini == ini')
