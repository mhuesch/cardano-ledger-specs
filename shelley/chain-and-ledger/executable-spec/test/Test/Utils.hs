{-# LANGUAGE PatternSynonyms #-}

module Test.Utils
  ( assertAll
  , mkKeyPair
  ) where

import           Data.Word (Word64)
import           Crypto.Random (drgNewTest, withDRG)
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           MockTypes (VKey, SKey)
import           Keys (pattern SKey, pattern VKey)

import           Hedgehog (MonadTest, (===))


assertAll :: (MonadTest m, Show a, Eq a) => (a -> Bool) -> [a] -> m ()
assertAll p xs = [] === filter (not . p) xs

mkKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKey, VKey)
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKey $ deriveVerKeyDSIGN sk)
