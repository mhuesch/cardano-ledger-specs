{-# LANGUAGE DeriveGeneric #-}

module Validation where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin (Coin)
import           Slot (Slot)

-- |Validation errors represent the failures of a transaction to be valid
-- for a given ledger state.
data ValidationError =
  -- | The transaction inputs are not valid.
    BadInputs
  -- | The transaction has expired
  | Expired Slot Slot
  -- | Pool Retirement Certificate expired
  | RetirementCertExpired Slot Slot
  -- | The transaction fee is too small
  | FeeTooSmall Coin Coin
  -- | Value is not conserved
  | ValueNotConserved Coin Coin
  -- | Unknown reward account
  | IncorrectRewards
  -- | One of the transaction witnesses is invalid.
  | InvalidWitness
  -- | The transaction does not have the required witnesses.
  | MissingWitnesses
  -- | Missing Replay Attack Protection, at least one input must be spent.
  | InputSetEmpty
  -- | A stake key cannot be registered again.
  | StakeKeyAlreadyRegistered
  -- | A stake key must be registered to be used or deregistered.
  | StakeKeyNotRegistered
  -- | The stake key to which is delegated is not known.
  | StakeDelegationImpossible
  -- | Stake pool not registered for key, cannot be retired.
  | StakePoolNotRegisteredOnKey
  -- | Other error (for conversion from STS errors that have no correspondance here)
  | UnknownValidationError
    deriving (Show, Eq, Generic)

instance NoUnexpectedThunks ValidationError

-- |The validity of a transaction, where an invalid transaction
-- is represented by list of errors.
data Validity = Valid | Invalid [ValidationError] deriving (Show, Eq)

instance Semigroup Validity where
  Valid <> b                 = b
  a <> Valid                 = a
  (Invalid a) <> (Invalid b) = Invalid (a ++ b)

instance Monoid Validity where
  mempty = Valid
  mappend = (<>)
