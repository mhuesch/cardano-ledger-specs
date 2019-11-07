{-# Language StandaloneDeriving #-}
{-# Language GADTs              #-}
module Test.Serialization where

--import Cardano.Prelude
--import Test.Cardano.Prelude

import Codec.CBOR.Encoding (encodeInt, Encoding (..), Tokens(..))
import Cardano.Binary (toCBOR, ToCBOR)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertEqual)


import Coin (Coin (..))
import BaseTypes (UnitInterval (..))
import Data.Ratio ((%))

import Data.Typeable (Typeable, typeOf)



data EncodingPair where
  EncodePair :: (Typeable a, ToCBOR a) => a -> Encoding -> EncodingPair

(#>) :: (ToCBOR a) => a -> Encoding -> EncodingPair
(#>) = EncodePair

checkEncoding :: EncodingPair -> TestTree
checkEncoding (EncodePair x t) = testCase testName $
  assertEqual typeName (fromEncoding t) (fromEncoding $ toCBOR x)
  where
   typeName = show (typeOf x)
   testName = "prop_serialize_" <> typeName

fromEncoding :: Encoding -> Tokens
fromEncoding (Encoding e) = e TkEnd

prop_serializeCoin :: Assertion
prop_serializeCoin =
  let coin = Coin 30
      tokens = encodeInt 30
   in assertEqual "" (fromEncoding tokens) $ (fromEncoding $ toCBOR coin)


serializationTests :: TestTree
serializationTests = testGroup "Serialization Tests" $ checkEncoding <$>
  [ Coin 30 #> Encoding (TkWord64 30)
  , UnsafeUnitInterval (1 % 2) #> Encoding (TkWord64 1 . TkWord64 2)
  ]

