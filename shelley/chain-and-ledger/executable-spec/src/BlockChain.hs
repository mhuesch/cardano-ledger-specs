{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module BlockChain
  ( HashHeader(..)
  , BHBody(..)
  , BHeader(..)
  , Block(..)
  , ProtVer(..)
  , TxSeq(..)
  , bhHash
  , bhbHash
  , bHeaderSize
  , bBodySize
  , slotToNonce
  , hBbsize
    -- accessor functions
  , bheader
  , bhbody
  , bbody
  , hsig
    --
  , slotsPrior
  , startRewards
  , seedEta
  , seedL
  , bvkcold
  , vrfChecks
  , incrBlocks
  , mkSeed
  )
where

import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Ratio (denominator, numerator)
import           Data.Sequence (Seq)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Cardano.Binary (ToCBOR (toCBOR), encodeListLen)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Cardano.Crypto.Hash (SHA256)
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Ledger.Shelley.Crypto
import           BaseTypes (Nonce (..), Seed(..), UnitInterval, intervalValue, mkNonce)
import           Delegation.Certificates (PoolDistr (..))
import           EpochBoundary (BlocksMade (..))
import           Keys (Hash, KESig, KeyHash,
                     VKey, VRFValue(..), hash, hashKey, hashKeyVRF)
import           OCert (OCert (..))
import           Slot (Duration, Slot (..), BlockNo(..))
import           Tx (Tx (..))

import           NonIntegral ((***))

-- |The hash of a Block Header
newtype HashHeader crypto =
  HashHeader (Hash (HASH crypto) (BHeader crypto))
  deriving (Show, Eq, Generic, Ord)

deriving instance Crypto crypto => ToCBOR (HashHeader crypto)

instance NoUnexpectedThunks (HashHeader crypto)

newtype TxSeq crypto
    = TxSeq (Seq (Tx crypto))
  deriving (Eq, Show)

instance Crypto crypto =>
  ToCBOR (TxSeq crypto)  where
  toCBOR (TxSeq s) = toCBOR $ toList s

-- | Hash of block body
newtype HashBBody crypto =
  HashBBody (Hash (HASH crypto) (TxSeq crypto))
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (HashBBody crypto)

-- |Hash a given block header
bhHash
  :: Crypto crypto
  => BHeader crypto
  -> HashHeader crypto
bhHash = HashHeader . hash

-- |Hash a given block body
bhbHash
  :: Crypto crypto
  => TxSeq crypto
  -> HashBBody crypto
bhbHash = HashBBody . hash

data BHeader crypto
  = BHeader
      (BHBody crypto)
      (KESig crypto (BHBody crypto))
  deriving (Show, Generic, Eq)

instance Crypto crypto
  => NoUnexpectedThunks (BHeader crypto)

instance Crypto crypto
  => ToCBOR (BHeader crypto)
 where
   toCBOR (BHeader bHBody kESig) =
     encodeListLen 2
       <> toCBOR bHBody
       <> toCBOR kESig

data ProtVer = ProtVer Natural Natural Natural
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks ProtVer

instance ToCBOR ProtVer where
  toCBOR (ProtVer x y z) =
     encodeListLen 3
       <> toCBOR x
       <> toCBOR y
       <> toCBOR z

data BHBody crypto = BHBody
  { -- | Hash of the previous block header
    -- The first block in a chain will set this field to Nothing.
    -- TODO Since the Shelley chain will begins with blocks from
    -- the Byron era, we should probably use a sum type here,
    -- so that the first shelley block can point to the last Byron block.
    bheaderPrev           :: HashHeader crypto
    -- | verification key of block issuer
  , bheaderVk             :: VKey crypto
    -- | VRF verification key for block issuer
  , bheaderVrfVk          :: VRF.VerKeyVRF (VRF crypto)
    -- | block slot
  , bheaderSlot           :: Slot
    -- | block number
  , bheaderBlockNo        :: BlockNo
    -- | block nonce
  , bheaderEta            :: VRF.CertifiedVRF (VRF crypto) Nonce
    -- | leader election value
  , bheaderL              :: VRF.CertifiedVRF (VRF crypto) UnitInterval
    -- | Size of the block body
  , bsize                 :: Natural
    -- | Hash of block body
  , bhash                 :: HashBBody crypto
    -- | operational certificate
  , bheaderOCert          :: OCert crypto
    -- | protocol version
  , bprotvert          :: ProtVer
  } deriving (Show, Eq, Generic)

instance Crypto crypto
  => NoUnexpectedThunks (BHBody crypto)

instance Crypto crypto
  => ToCBOR (BHBody crypto)
 where
  toCBOR bhBody =
    encodeListLen 10
      <> toCBOR (bheaderPrev bhBody)
      <> toCBOR (bheaderVk bhBody)
      <> VRF.encodeVerKeyVRF (bheaderVrfVk bhBody)
      <> toCBOR (bheaderSlot bhBody)
      <> toCBOR (bheaderEta bhBody)
      <> toCBOR (bheaderL bhBody)
      <> toCBOR (bsize bhBody)
      <> toCBOR (bhash bhBody)
      <> toCBOR (bheaderOCert bhBody)
      <> toCBOR (bprotvert bhBody)

data Block crypto
  = Block
    (BHeader crypto)
    (TxSeq crypto)
  deriving (Show, Eq)

bHeaderSize
  :: ( Crypto crypto)
  => BHeader crypto
  -> Int
bHeaderSize = BS.length . BS.pack . show

bBodySize :: Crypto crypto => TxSeq crypto-> Int
bBodySize (TxSeq txs) = sum (map (BS.length . BS.pack . show) $ toList txs)

slotToNonce :: Slot -> Nonce
slotToNonce (Slot s) = mkNonce (fromIntegral s)

bheader
  :: Block crypto
  -> BHeader crypto
bheader (Block bh _) = bh

bbody :: Block crypto -> TxSeq crypto
bbody (Block _ txs) = txs

bhbody
  :: BHeader crypto
  -> BHBody crypto
bhbody (BHeader b _) = b

hsig
  :: BHeader crypto
  -> KESig crypto (BHBody crypto)
hsig (BHeader _ s) = s

slotsPrior :: Duration
slotsPrior = 33 -- one third of slots per epoch

startRewards :: Duration
startRewards = 33 -- see above

-- | Construct a seed to use in the VRF computation.
mkSeed
  :: Crypto crypto
  => Nonce -- ^ Universal constant
  -> Slot
  -> Nonce -- ^ Epoch nonce
  -> HashHeader crypto
  -> Seed
mkSeed (Nonce uc) slot nonce lastHash =
  Seed . coerce $ uc `Hash.xor` coerce (hash @SHA256 (slot, nonce, lastHash))
mkSeed NeutralNonce slot nonce lastHash =
  Seed . coerce $ (hash @SHA256 (slot, nonce, lastHash))


vrfChecks
  ::  forall crypto
  . ( Crypto crypto
    , VRF.Signable (VRF crypto) Seed
    )
  => Nonce
  -> PoolDistr crypto
  -> UnitInterval
  -> BHBody crypto
  -> Bool
vrfChecks eta0 (PoolDistr pd) f bhb =
  let sigma' = Map.lookup hk pd
  in  case sigma' of
        Nothing -> False
        Just (sigma, vrfHK) ->
          vrfHK == hashKeyVRF @crypto vrfK
            && VRF.verifyCertified vrfK
                         (mkSeed seedEta slot eta0 prevHash)
                         (coerce $ bheaderEta bhb)
            && VRF.verifyCertified vrfK
                         (mkSeed seedL slot eta0 prevHash)
                         (coerce $ bheaderL bhb)
            && intervalValue (fromNatural . VRF.certifiedNatural $ bheaderL bhb)
            <  1
            -  ((1 - activeSlotsCoeff) *** fromRational sigma)
 where
  hk = hashKey $ bvkcold bhb
  vrfK = bheaderVrfVk bhb
  prevHash = bheaderPrev bhb
  slot = bheaderSlot bhb
  f' = intervalValue f
  activeSlotsCoeff =
    fromIntegral (numerator f') / fromIntegral (denominator f')

seedEta :: Nonce
seedEta = mkNonce 0

seedL :: Nonce
seedL = mkNonce 1

bvkcold :: BHBody crypto -> VKey crypto
bvkcold bhb = ocertVkCold $ bheaderOCert bhb

hBbsize :: BHBody crypto -> Natural
hBbsize = bsize

incrBlocks
  :: Bool
  -> KeyHash crypto
  -> BlocksMade crypto
  -> BlocksMade crypto
incrBlocks isOverlay hk b'@(BlocksMade b)
  | isOverlay = b'
  | otherwise = BlocksMade $ case hkVal of
    Nothing -> Map.insert hk 1 b
    Just n  -> Map.insert hk (n + 1) b
  where hkVal = Map.lookup hk b
