{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Tests.TestRevive (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CacheBehaviors
import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompEval
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Tests.TestHelper
import Control.Computations.CompEngine.Types
import Control.Computations.CompEngine.Utils.PriorityAgingQueue (PaqPriority (..))
import Control.Computations.FlowImpls.HashMapFlow
import Control.Computations.Utils.Logging

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Test.Framework

test_dontDeleteAfterRevival :: IO ()
test_dontDeleteAfterRevival = runCompEngineTest compDefs shouldStartNextRun () doTest
 where
  compDefs =
    do
      writeComp <- wireComp writeCompDef
      intermediateComp <- wireComp (intermediateCompDef writeComp)
      wireComp (mainCompDef writeComp intermediateComp)
   where
    isYes :: Maybe BS.ByteString -> Bool
    isYes = (Just "yes" ==)
    mainCompDef
      :: Comp () ()
      -> Comp () ()
      -> CompDef () ()
    mainCompDef writeComp intermediateComp =
      defineComp "main" inMemoryShowCaching $ \() ->
        do
          doSecond <- liftM isYes (get "second")
          evalCompOrFail intermediateComp ()
          when doSecond (evalCompOrFail writeComp ())
          return ()
    intermediateCompDef :: Comp () () -> CompDef () ()
    intermediateCompDef writeComp =
      defineComp "intermediate" inMemoryShowCaching $ \() ->
        do
          doFirst <- liftM isYes (get "first")
          when doFirst (evalCompOrFail writeComp ())
    writeCompDef :: CompDef () ()
    writeCompDef =
      defineComp "write" inMemoryShowCaching $ \() ->
        put "foo" "bar"
  doTest hmf =
    do
      mRes <- hmfLookup hmf "foo"
      assertEqual (Just "bar") mRes
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ _ s
    | run == 1 =
        do
          logInfo "setting first=yes and second=no"
          hmfInsert hmf "first" "yes"
          hmfInsert hmf "second" "no"
          return (startNextRun, s)
    | run == 2 =
        do
          logInfo "setting first=no and second=yes"
          hmfInsert hmf "first" "no"
          hmfInsert hmf "second" "yes"
          return (startNextRun, s)
    | otherwise =
        return (noNextRun, s)

{- | The counterexample that rules out an "obvious" but wrong fix for
 'tellGarbage''s revival bookkeeping (see 'CapSeq'\'s haddock in Impl.hs):
 track revived caps in one set, freed caps in another, and subtract the
 revived set from the freed set once at the end. That gets
 'test_dontDeleteAfterRevival' above right by accident -- free-then-revive:
 additively, "freed at some point" and "revived at some point" are both
 true, and subtracting unconditionally erases the cap from the freed set,
 which happens to match "don't delete" here -- but is wrong on the mirror
 case, entirely within one accumulation window: a cap revived *first*,
 then freed *later* in the same round. Order doesn't factor into an
 unconditional subtraction at all, so the additive scheme still erases the
 cap from the freed set, and its now-dead output is never deleted --
 wrong, and silently so.

 @write@ here is revived, not merely evaluated for the first time: its
 revival in run 2 is driven directly by the engine's own stale-queue
 draining (real-time priority guarantees it is dequeued and re-run before
 @keeper@, which is regular priority, whenever both are stale in the same
 round -- see 'PaqRealTime's haddock), entirely independent of any caller
 depending on it. @keeper@, evaluated immediately after, drops the only
 dependency on @write@ that was keeping it alive, freeing it. Revive, then
 free, both within the one round run 2's changes below drive -- the shape
 a correct implementation must still get right, and the one an additive
 fix gets wrong (verified: swapping 'garbage'\'s revival check for the
 additive scheme described above makes this assertion fail).
-}
test_reviveThenFreeInSameRoundDeletesOutput :: IO ()
test_reviveThenFreeInSameRoundDeletesOutput = runCompEngineTest compDefs shouldStartNextRun () doTest
 where
  compDefs =
    do
      writeComp <- wireComp writeCompDef
      wireComp (keeperCompDef writeComp)
   where
    isYes :: Maybe BS.ByteString -> Bool
    isYes = (Just "yes" ==)
    keeperCompDef :: Comp () () -> CompDef () ()
    keeperCompDef writeComp =
      defineComp "keeper" inMemoryShowCaching $ \() ->
        do
          keepIt <- liftM isYes (get "keepIt")
          when keepIt (evalCompOrFail writeComp ())
    -- Real-time priority: whenever both this cap and "keeper" are stale in
    -- the same round, the stale queue serves this one first no matter what
    -- order the two were enqueued in -- see PaqPriority's haddock ("will be
    -- served first no matter what"). That is what pins revive-then-free down
    -- deterministically instead of leaving it to the queue's insertion-order
    -- tie-break.
    writeCompDef :: CompDef () ()
    writeCompDef =
      defineCompWithPriority PaqRealTime (T.pack "write") inMemoryShowCaching $ \() ->
        do
          _ <- get "flagX"
          put "foo" "bar"
  doTest hmf =
    do
      mRes <- hmfLookup hmf "foo"
      assertEqual Nothing mRes
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun hmf run _ _ s
    | run == 1 =
        do
          logInfo "setting keepIt=yes and flagX=1: write gets its first result"
          hmfInsert hmf "keepIt" "yes"
          hmfInsert hmf "flagX" "1"
          return (startNextRun, s)
    | run == 2 =
        do
          logInfo "flagX revives write (real-time, ahead of keeper); keepIt=no then frees it"
          hmfInsert hmf "flagX" "2"
          hmfInsert hmf "keepIt" "no"
          return (startNextRun, s)
    | otherwise =
        return (noNextRun, s)
