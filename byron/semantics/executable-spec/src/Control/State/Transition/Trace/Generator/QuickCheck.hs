{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.State.Transition.Trace.Generator.QuickCheck
  ( HasTrace (envGen, sigGen, shrinkSignal)
  , traceFrom
  , trace
  , shrinkTrace
  -- * Trace generator properties
  , forAllTrace
  , onlyValidSignalsAreGenerated
  -- * Trace classification
  , traceLengthsAreClassified
  , classifyTraceLength
  , classifySize
  -- * Internal
  , mkIntervals
  )
where

import           Data.Word (Word64)
import qualified Test.QuickCheck as QuickCheck

import           Control.State.Transition (Environment, IRC (IRC), STS, Signal, State, TRC (TRC),
                     applySTS)
import qualified Control.State.Transition as STS
import           Control.State.Transition.Trace (Trace)
import qualified Control.State.Transition.Trace as Trace


-- | State transition systems for which traces can be generated, given a trace
-- generation environment.
--
-- The trace generation environment allows to pass relevant data to the trace
-- generation algorithm.
class STS sts => HasTrace sts traceGenEnv where

  envGen :: traceGenEnv -> QuickCheck.Gen (Environment sts)

  sigGen
    :: traceGenEnv
    -> Environment sts
    -> State sts
    -> QuickCheck.Gen (Signal sts)

  shrinkSignal :: Signal sts -> [Signal sts]

-- | Generate a random trace starting in the given environment and initial state.
traceFrom
  :: forall sts traceGenEnv
   . (HasTrace sts traceGenEnv)
  => Word64
  -- ^ Maximum trace length.
  -> traceGenEnv
  -> Environment sts
  -> State sts
  -> QuickCheck.Gen (Trace sts)
traceFrom maxTraceLength traceGenEnv env st0 = do
  chosenTraceLength <- QuickCheck.choose (0, maxTraceLength)
  Trace.mkTrace env st0 <$> loop chosenTraceLength st0 []
  where
    loop
      :: Word64
      -> State sts
      -> [(State sts, Signal sts)]
      -> QuickCheck.Gen [(State sts, Signal sts)]
    loop 0 _ acc = pure $! acc
    loop !d sti stSigs = do
      sig <- sigGen @sts @traceGenEnv traceGenEnv env sti
      case STS.applySTS @sts (TRC(env, sti, sig)) of
        Left _predicateFailures ->
          loop (d - 1) sti stSigs
        Right sti' ->
          loop (d - 1) sti' ((sti', sig): stSigs)

-- | Generate a random trace.
trace
  :: forall sts traceGenEnv
   . (HasTrace sts traceGenEnv, Show (Environment sts))
  => Word64
  -- ^ Maximum trace length.
  -> traceGenEnv
  -> QuickCheck.Gen (Trace sts)
trace maxTraceLength traceGenEnv = do
  env <- envGen @sts @traceGenEnv traceGenEnv
  case STS.applySTS @sts (IRC env) of
    Left pf -> error $ "Failed to apply the initial rule to the generated environment.\n"
                     ++ "Generated environment: " ++ show env
                     ++ "Failure: " ++ show pf
    Right st0 -> traceFrom maxTraceLength traceGenEnv env st0

-- | Check a property on the 'sts' traces.
forAllTrace
  :: forall sts traceGenEnv prop
   . ( HasTrace sts traceGenEnv
     , QuickCheck.Testable prop
     , Show (Environment sts)
     , Show (State sts)
     , Show (Signal sts)
     )
  => Word64
  -- ^ Maximum trace length.
  -> traceGenEnv
  -> (Trace sts -> prop)
  -> QuickCheck.Property
forAllTrace maxTraceLength traceGenEnv prop =
  QuickCheck.forAllShrink
    (trace @sts @traceGenEnv maxTraceLength traceGenEnv)
    (shrinkTrace @sts @traceGenEnv)
    prop

-- | See 'Test.QuickCheck.shrink'.
shrinkTrace
  :: forall sts traceGenEnv
   . (HasTrace sts traceGenEnv) => Trace sts -> [Trace sts]
shrinkTrace tr = Trace.closure env st0 <$> traceSignalsShrinks
  where
    env = Trace._traceEnv tr
    st0 = Trace._traceInitState tr
    signals = Trace.traceSignals Trace.OldestFirst tr
    traceSignalsShrinks =
      QuickCheck.shrinkList (shrinkSignal @sts @traceGenEnv) signals

-- | Property that asserts that only valid signals are generated.
onlyValidSignalsAreGenerated
  :: forall sts traceGenEnv
   . ( HasTrace sts traceGenEnv
     , Show (Environment sts)
     , Show (State sts)
     , Show (Signal sts)
     )
  => Word64
  -- ^ Maximum trace length.
  -> traceGenEnv
  -> QuickCheck.Property
onlyValidSignalsAreGenerated maxTraceLength traceGenEnv =
  forAllTrace maxTraceLength traceGenEnv validSignalsAreGenerated
  where
    validSignalsAreGenerated
      :: Trace sts
      -> QuickCheck.Property
    validSignalsAreGenerated someTrace =
      QuickCheck.forAllShrink
      (sigGen @sts @traceGenEnv traceGenEnv env lastState)
      (shrinkSignal @sts @traceGenEnv)
      signalIsValid
      where
        signalIsValid signal =
          case applySTS @sts (TRC (env, lastState, signal)) of
            Left pf -> QuickCheck.counterexample (show (signal, pf)) False
            Right _ -> QuickCheck.property True
        env = Trace._traceEnv someTrace
        lastState = Trace.lastState someTrace

--------------------------------------------------------------------------------
-- Trace classification
--------------------------------------------------------------------------------

-- | Property that simply classifies the lengths of the generated traces.
traceLengthsAreClassified
  :: forall sts traceGenEnv
   . ( HasTrace sts traceGenEnv
     , Show (Environment sts)
     , Show (State sts)
     , Show (Signal sts)
     )
  => Word64
  -- ^ Maximum trace length that the signal generator of 's' can generate.
  -> Word64
  -- ^ Lengths of the intervals in which the lengths range should be split.
  -> traceGenEnv
  -- ^ Trace generation environment
  -> QuickCheck.Property
traceLengthsAreClassified maxTraceLength intervalSize traceGenEnv =
  forAllTrace @sts maxTraceLength traceGenEnv (classifyTraceLength maxTraceLength intervalSize)

-- | Classify the trace length as either:
--
-- - being empty
-- - being a singleton
-- - having the given maximum size
-- - belonging to one of the intervals between 2 and the maximum size - 1. The
--   number of intervals are determined by the @step@ parameter.
--
classifyTraceLength
  :: Word64
  -- ^ Maximum size of the traces
  -> Word64
  -- ^ Steps used to divide the interval
  -> Trace s
  -> QuickCheck.Property
classifyTraceLength maxTraceLength step tr =
  classifySize "trace length:" tr (fromIntegral . Trace.traceLength) maxTraceLength step

-- | Classify the value size as either:
--
-- - being empty
-- - being a singleton
-- - having the given maximum size
-- - belonging to one of the intervals between 2 and the maximum size - 1. The
--   number of intervals are determined by the @step@ parameter.
--
classifySize
  :: (Ord n, Show n, Integral n)
  => String
  -- ^ Prefix to be added to the label intervals
  -> a
  -- ^ Value to classify
  -> (a -> n)
  -- ^ Size function
  -> n
  -- ^ Maximum value size
  -> n
  -- ^ Steps used to divide the size interval
  -> QuickCheck.Property
classifySize prefixLabel value sizeF upBound step =
  QuickCheck.classify (sizeF value == 0) (mkLabel "empty")      $
  QuickCheck.classify (sizeF value == 1) (mkLabel "singleton")  $
  QuickCheck.classify (sizeF value == upBound) upBoundLabel $
  foldr classifySizeInterval (QuickCheck.property True) (mkIntervals 2 (upBound - 1) step)
  where
    upBoundLabel = mkLabel $ show upBound
    mkLabel = ((prefixLabel ++ " ") ++)
    classifySizeInterval (low, high) prop =
      QuickCheck.classify (low <= sizeF value && sizeF value < high) desc prop
      where
        desc = "[" ++ show low ++ ", " ++ show high ++ ")"

-- | Given a lower bound @low@,  an upper bound @high@ and a step size @step@
-- (both of which must be positive), divide the interval @[0, ub]@ into
-- sub-intervals of @step@ size.
--
-- If any of these values is negative the empty list will be returned.
--
-- Examples:
--
-- >>> mkIntervals 0 0 0 :: [(Int, Int)]
-- []
--
-- >>> mkIntervals 0 10 2 :: [(Int, Int)]
-- [(0,2),(2,4),(4,6),(6,8),(8,10)]
--
-- >>> mkIntervals 1 10 2 :: [(Int, Int)]
-- [(1,3),(3,5),(5,7),(7,9),(9,10)]
--
--
-- >>> mkIntervals 3 10 3 :: [(Int, Int)]
-- [(3,6),(6,9),(9,10)]
--
-- >>> mkIntervals 5 2 3 :: [(Int, Int)]
-- []
--
-- >>> mkIntervals (-1) 10 3 :: [(Int, Int)]
-- []
--
-- >>> mkIntervals 1 (-10) 3 :: [(Int, Int)]
-- []
--
-- >>> mkIntervals 1 1000 (-100) :: [(Int, Int)]
-- []
--
mkIntervals
  :: Integral n
  => n
  -- ^ Interval lower bound
  -> n
  -- ^ Interval upper bound
  -> n
  -- ^ Step size, used to divide the interval in sub-intervals of the same
  -- length.
  -> [(n, n)]
mkIntervals low high step
  | 0 <= low && low <= high && 0 < step =
    [(low + i * step, high `min` (low + (i + 1) * step)) | i <- [0 .. n - 1]]
  | otherwise = []
  where
    highNorm = high - low
    n = highNorm `div` step + 1 `min` (highNorm `mod` step)
