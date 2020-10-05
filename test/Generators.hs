{-# LANGUAGE TypeApplications #-}
module Generators where
import Control.Monad (when)
import Data.Char (isPrint, isSpace)
import Data.List (foldl')
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.String ( IsString(fromString) )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Test.QuickCheck
    ( discard, frequency, listOf, listOf1, oneof, suchThat, Arbitrary(..), Gen )
import Data.Tini ( empty, fromList, remove, set, toList, Ini, Key )

genKeyName :: Gen String
genKeyName = do
  s <- listOf1 $ arbitrary `suchThat` \c -> and
    [ isPrint c
    , not (c `elem` ".;#=[]")
    ]
  if all isSpace s
    then discard
    else return s

genKey :: Gen Key
genKey = frequency
  [ (1, fromString <$> genKeyName)
  , (2, do
      sec <- genKeyName
      key <- genKeyName
      pure $ fromString $ concat [sec, ".", key]
    )
  ]

genSetter :: Gen (Ini -> Key -> Ini)
genSetter = oneof
  [ arbitrary @Int >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Int8 >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Int16 >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Int32 >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Int64 >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Word >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Word8 >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Word16 >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Word32 >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Word64 >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Integer >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Float >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Double >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Char >>= \x -> pure (\i k -> set i k x)
  , arbitrary @Bool >>= \x -> pure (\i k -> set i k x)
  , arbitrary @String >>= \x -> pure (\i k -> set i k x)
  , arbitrary @[String] >>= \x -> pure (\i k -> set i k x)
  , arbitrary @[Int] >>= \x -> pure (\i k -> set i k x)
  , arbitrary @(Maybe String) >>= \x -> pure (\i k -> set i k x)
  , arbitrary @(Maybe Int) >>= \x -> pure (\i k -> set i k x)
  , arbitrary @(Int, Int) >>= \x -> pure (\i k -> set i k x)
  , arbitrary @(Int, String) >>= \x -> pure (\i k -> set i k x)
  , arbitrary @(Int, String, Maybe Char) >>= \x -> pure (\i k -> set i k x)
  ]

genEntry :: Gen (Ini -> Ini)
genEntry = do
  key <- genKey
  set_value <- genSetter
  return (flip set_value key)

instance Arbitrary Key where
  arbitrary = genKey
  shrink = map fromString
    . filter (not . all (\x -> isSpace x || x == '.'))
    . shrink
    . show

instance Arbitrary Ini where
  arbitrary = foldl' (\ini f -> f ini) empty <$> listOf genEntry
  shrink ini = 
    case prop_list of
      [(k, v)] -> [ fromList [(k', v')] | k' <- shrink k, v' <- shrink v ]
      _        -> [ remove key ini | (key, _) <- prop_list ]
    where
      prop_list = toList ini