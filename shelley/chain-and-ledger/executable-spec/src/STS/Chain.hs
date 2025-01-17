{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Chain
  ( CHAIN
  , ChainState (..)
  , PredicateFailure(..)
  , totalAda
  )
where

import qualified Data.Map.Strict as Map
import           Numeric.Natural (Natural)

import           BaseTypes
import           BlockChain
import           Coin (Coin (..))
import           Keys
import           LedgerState
import           OCert
import           Slot
import           Tx
import           UTxO (balance)

import qualified Cardano.Crypto.VRF as VRF
import           Control.State.Transition
import           Cardano.Ledger.Shelley.Crypto

import           STS.Bbody
import           STS.Bhead
import           STS.Prtcl

data CHAIN crypto

data ChainState crypto
  = ChainState
    { chainNes            :: NewEpochState crypto
    , chainOCertIssue     :: Map.Map (KeyHash crypto) Natural
    , chainEpochNonce     :: Nonce
    , chainEvolvingNonce  :: Nonce
    , chainCandidateNonce :: Nonce
    , chainHashHeader     :: HashHeader crypto
    , chainSlot           :: Slot
    }
  deriving (Show, Eq)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , Signable (DSIGN crypto) (TxBody crypto)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => STS (CHAIN crypto)
 where
  type State (CHAIN crypto)
    = ChainState crypto

  type Signal (CHAIN crypto)
    = Block crypto

  type Environment (CHAIN crypto) = Slot

  data PredicateFailure (CHAIN crypto)
    = BbodyFailure (PredicateFailure (BBODY crypto))
    | BheadFailure (PredicateFailure (BHEAD crypto))
    | PrtclFailure (PredicateFailure (PRTCL crypto))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [chainTransition]

chainTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
     , Signable (DSIGN crypto) (TxBody crypto)
     , KESignable crypto (BHBody crypto)
     , VRF.Signable (VRF crypto) Seed
     )
  => TransitionRule (CHAIN crypto)
chainTransition = do
  TRC (sNow, ChainState nes cs eta0 etaV etaC h sL, block@(Block bh _)) <- judgmentContext

  let gkeys = getGKeys nes
  nes' <-
    trans @(BHEAD crypto) $ TRC (BheadEnv gkeys, nes, bh)

  let NewEpochState e1 _ _ _ _ _ _ = nes
      NewEpochState e2 _ bcur es _ _pd osched = nes'
  let EpochState (AccountState _ _reserves) _ ls pp                         = es
  let LedgerState _ (DPState (DState _ _ _ _ _ _genDelegs _) (PState _ _ _)) _ = ls

  PrtclState cs' h' sL' eta0' etaV' etaC' <- trans @(PRTCL crypto)
    $ TRC ( PrtclEnv pp osched _pd _genDelegs sNow (e1 /= e2)
          , PrtclState cs h sL eta0 etaV etaC
          , bh)

  BbodyState ls' bcur' <- trans @(BBODY crypto)
    $ TRC (BbodyEnv (Map.keysSet osched) pp _reserves, BbodyState ls bcur, block)

  let nes'' = updateNES nes' bcur' ls'

  pure $ ChainState nes'' cs' eta0' etaV' etaC' h' sL'

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , Signable (DSIGN crypto) (TxBody crypto)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (BBODY crypto) (CHAIN crypto)
 where
  wrapFailed = BbodyFailure

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , Signable (DSIGN crypto) (TxBody crypto)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (BHEAD crypto) (CHAIN crypto)
 where
  wrapFailed = BheadFailure

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , Signable (DSIGN crypto) (TxBody crypto)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (PRTCL crypto) (CHAIN crypto)
 where
  wrapFailed = PrtclFailure

-- |Calculate the total ada in the chain state
totalAda :: ChainState crypto -> Coin
totalAda (ChainState nes _ _ _ _ _ _) =
  treasury_ + reserves_ + rewards_ + circulation + deposits + fees_
  where
    (EpochState (AccountState treasury_ reserves_) _ ls _) = nesEs nes
    (UTxOState u deposits fees_ _) = _utxoState ls
    (DPState ds _) = _delegationState ls
    rewards_ = sum (Map.elems (_rewards ds))
    circulation = balance u
