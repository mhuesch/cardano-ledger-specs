{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Serialization where

--import Cardano.Prelude
--import Test.Cardano.Prelude

import           Cardano.Binary (ToCBOR, toCBOR)
import           Cardano.Crypto.Hash (getHash)
import           Codec.CBOR.Encoding (Encoding (..), Tokens (..), encodeInt)
import           Data.ByteString (ByteString)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase)


import           BaseTypes (UnitInterval (..))
import           Coin (Coin (..))
import           Data.Ratio ((%))
import           Keys (pattern KeyHash, hashKey)
import           LedgerState (genesisId)
import           Slot (Slot (..))
import           Test.Utils
import           Delegation.Certificates (pattern RegKey)
import           TxData (pattern AddrBase, pattern AddrEnterprise, pattern AddrPtr, Credential (..),
                     Ptr (..), pattern Tx, pattern TxBody, pattern TxIn, pattern TxOut, _TxId)
import           Updates (emptyUpdate)

import           MockTypes (Addr, KeyHash, Tx, TxBody, TxId, TxIn, TxOut)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Typeable (Typeable, typeOf)



data EncodingPair where
  EncodePair :: (Typeable a, ToCBOR a) => a -> Encoding -> EncodingPair

(#>) :: (ToCBOR a) => a -> Encoding -> EncodingPair
(#>) = EncodePair

checkEncoding :: EncodingPair -> TestTree
checkEncoding (EncodePair x t) = testCase testName $
  assertEqual typeName (fromEncoding t) (fromEncoding $ toCBOR x)
  where
   typeName = show (typeOf x)
   testName = "prop_serialize_" <> map (\c -> if c == ' ' then '-' else c) typeName

fromEncoding :: Encoding -> Tokens
fromEncoding (Encoding e) = e TkEnd

prop_serializeCoin :: Assertion
prop_serializeCoin =
  let coin = Coin 30
      tokens = encodeInt 30
   in assertEqual "" (fromEncoding tokens) (fromEncoding $ toCBOR coin)

getRawKeyHash :: KeyHash -> ByteString
getRawKeyHash (KeyHash hsh) = getHash hsh

getRawTxId :: TxId -> ByteString
getRawTxId = getHash . _TxId

testKeyHash1 :: KeyHash
testKeyHash1 = (hashKey . snd . mkKeyPair) (0, 0, 0, 0, 1)

testKeyHash2 :: KeyHash
testKeyHash2 = (hashKey . snd . mkKeyPair) (0, 0, 0, 0, 2)

testAddrE :: Addr
testAddrE = AddrEnterprise (KeyHashObj testKeyHash1)

testTxB :: TxBody
testTxB = TxBody
           (Set.fromList [TxIn genesisId 1]) --
           [TxOut testAddrE (Coin 2)]
           (Seq.fromList ([ RegKey (KeyHashObj testKeyHash1) ]))
           Map.empty
           (Coin 9)
           (Slot 500)
           emptyUpdate

testTx :: Tx
testTx = Tx testTxB Set.empty Map.empty

serializationTests :: TestTree
serializationTests = testGroup "Serialization Tests" $ checkEncoding <$>
  [ Coin 30 #> Encoding (TkWord64 30)
  , UnsafeUnitInterval (1 % 2) #> Encoding (TkWord64 1 . TkWord64 2)
  , Slot 7 #> Encoding (TkWord64 7)
  , testKeyHash1 #> Encoding (TkBytes (getRawKeyHash testKeyHash1))
  , KeyHashObj testKeyHash1 #>
      Encoding (TkListLen 2 . TkWord 0 . TkBytes (getRawKeyHash testKeyHash1))
  , AddrBase (KeyHashObj testKeyHash1) (KeyHashObj testKeyHash2) #>
      Encoding (TkListLen 5 . TkWord 0
        . TkWord 0 . TkBytes (getRawKeyHash testKeyHash1)
        . TkWord 0 . TkBytes (getRawKeyHash testKeyHash2))
  , AddrPtr (KeyHashObj testKeyHash1) (Ptr (Slot 12) 0 3) #>
      Encoding (TkListLen 6 . TkWord 1
        . TkWord 0 . TkBytes (getRawKeyHash testKeyHash1)
        . TkWord64 12 . TkInteger 0 . TkInteger 3)
  , AddrEnterprise (KeyHashObj testKeyHash1) #>
      Encoding (TkListLen 3 . TkWord 2
        . TkWord 0 . TkBytes (getRawKeyHash testKeyHash1))
  , (TxIn genesisId 0 :: TxIn) #>
      Encoding (TkListLen 2 . TkBytes (getRawTxId genesisId) . TkInteger 0)
  , (TxOut (AddrEnterprise (KeyHashObj testKeyHash1)) (Coin 2) :: TxOut) #>
      Encoding (TkListLen 4 . TkWord 2 . TkWord 0
        . TkBytes (getRawKeyHash testKeyHash1) . TkWord64  2)
  -- , testTxB #> -- 212 is (getRawTxId genesisId) v9 is (getRawKeyHash testKeyHash1)
  --     Encoding (TkListLen 6 . TkTag 258 . TkListLen 1 . TkListLen 2
  --       . TkBytes (getRawTxId genesisId) . TkInteger 1 . TkListBegin
  --       . TkListLen 2 . TkListLen 2 . TkWord 1 . TkListLen 2 . TkWord 1
  --       . TkBytes (getRawKeyHash testKeyHash1) . TkWord64 2 . TkBreak
  --       . TkListBegin . TkListLen 2 . TkWord 0 . TkListLen 2
  --       . TkWord 1 . TkBytes (getRawKeyHash testKeyHash1) . TkBreak . TkMapLen 0 . TkWord64 9
  --       . TkWord64 500 . TkListLen 2 . TkMapLen 0 . TkMapLen 0)
  --Below is not ready
  -- , testTx #>
  --     Encoding (TkListLen 2 . TkListLen 6 . TkTag 258 . TkListLen 1 . TkListLen 2
  --       . TkBytes (getRawTxId genesisId) . TkInteger 1 . TkListBegin . TkListLen 2 . TkListLen 2
  --       . TkWord 1 . TkListLen 2 . TkWord 1 . TkBytes (getRawKeyHash testKeyHash1) . TkWord64 2
  --       . TkBreak . TkListBegin . TkListLen 2 . TkWord 0 . TkListLen 2 . TkWord 1
  --       . TkBytes (getRawKeyHash testKeyHash1) . TkBreak . TkMapLen 0 . TkWord64 9 . TkWord64 500
  --       . TkListLen 2 . TkMapLen 0 . TkMapLen 0 . TkTag 258 . TkListLen 0 . TkMapLen 0)
  ]

