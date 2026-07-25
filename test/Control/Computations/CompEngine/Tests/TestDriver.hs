{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Regression coverage for the settle-detection race fixed in the
 @bench@ demo (see @bench/Control/Computations/Demos/Bench/Main.hs@):
 'waitForRunAtLeast' only guarantees @rs_run result >= n@, never equality,
 because 'Control.Computations.CompEngine.Driver.compDriver'' posts a run's
 'RunStats' -- tagged with the *upcoming* run number, describing the
 *previous* run's leftover 'rs_staleCaps' -- before that upcoming run has
 attempted its own blocking wait for changes. A caller can observe an
 "overshot" run number whose 'rs_staleCaps' is already 0, i.e. already
 fully settled.

 These tests reproduce that overshoot deterministically -- by writing a
 pre-settled 'RunStats' directly into the 'TVar', with no engine, no
 threads, no timing dependence -- and check both directions:

   * the *buggy* pattern (advance blindly to @rs_run result + 1@ from an
     overshot observation) genuinely hangs: nothing will ever post that run
     number, since 'waitForFullSettle' is deliberately never given a chance
     to run and no mutation ever occurs. Bounded with 'timeout' so the test
     itself cannot hang.
   * the *fixed* pattern (re-poll starting from the observed run's own
     number, via 'waitForFullSettle') returns immediately, without
     blocking, because it re-checks 'rs_staleCaps' before ever assuming a
     newer run is needed.
-}
module Control.Computations.CompEngine.Tests.TestDriver (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.Driver
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import System.Timeout (timeout)
import Test.Framework

-- | A driver-posted 'RunStats' that's already fully settled (0 leftover
-- stale caps) but whose 'rs_run' is ahead of what a caller who only asked
-- for "at least run 1" would naively expect.
overshotSettledStats :: RunStats
overshotSettledStats = RunStats{rs_run = 2, rs_hadChanges = True, rs_staleCaps = 0}

-- | Nothing ever writes to this 'TVar' again in these tests: it models the
-- driver thread having posted its one settle signal and then genuinely
-- blocking (in 'Control.Computations.FlowImpls.HashMapFlow.waitChangesImpl')
-- with no further mutation ever coming -- exactly the state the real
-- deadlock was caught in.
mkFrozenAtOvershoot :: IO (TVar (Option RunStats))
mkFrozenAtOvershoot = newTVarIO (Some overshotSettledStats)

test_waitForRunAtLeastCanOvershoot :: IO ()
test_waitForRunAtLeastCanOvershoot =
  do
    runVar <- mkFrozenAtOvershoot
    -- Asking for "at least run 1" is satisfied immediately by the
    -- already-posted run 2 -- the "at least" contract, not "exactly".
    rs <- waitForRunAtLeast runVar 1
    assertEqual 2 (rs_run rs)
    assertEqual 0 (rs_staleCaps rs)

test_blindlyAdvancingPastAnOvershotSettleDeadlocks :: IO ()
test_blindlyAdvancingPastAnOvershotSettleDeadlocks =
  do
    runVar <- mkFrozenAtOvershoot
    rs1 <- waitForRunAtLeast runVar 1
    -- The bug this test guards against: computing "the next run" as
    -- @rs_run rs1 + 1@ from an overshot observation, instead of re-polling
    -- from 'rs1's own run number. Nothing will ever post run 3 here, so
    -- this must hang -- proven via a short bounded 'timeout' rather than
    -- actually waiting forever.
    result <- timeout 500000 (waitForFullSettle runVar (rs_run rs1 + 1))
    case result of
      Nothing -> pure () -- expected: genuinely hung until the timeout fired
      Just rs -> assertFailure ("waitForFullSettle unexpectedly returned run " ++ show (rs_run rs))

test_pollingFromTheObservedRunSettlesImmediately :: IO ()
test_pollingFromTheObservedRunSettlesImmediately =
  do
    runVar <- mkFrozenAtOvershoot
    rs1 <- waitForRunAtLeast runVar 1
    -- The fix: seed 'waitForFullSettle' with 'rs1's own (possibly-overshot)
    -- run number. It re-checks 'rs_staleCaps' itself, so it returns
    -- immediately here instead of waiting for a run number that will never
    -- come.
    result <- timeout 500000 (waitForFullSettle runVar (rs_run rs1))
    case result of
      Nothing -> assertFailure "waitForFullSettle unexpectedly blocked"
      Just rs2 -> do
        assertEqual 2 (rs_run rs2)
        assertEqual 0 (rs_staleCaps rs2)

test_waitForFullSettlePollsForwardWhenGenuinelyUnsettled :: IO ()
test_waitForFullSettlePollsForwardWhenGenuinelyUnsettled =
  do
    runVar <- newTVarIO (Some RunStats{rs_run = 1, rs_hadChanges = True, rs_staleCaps = 3})
    -- Simulate the driver posting the next run, fully settled, slightly
    -- later (as it would after actually draining those 3 stale caps).
    _ <- forkDelayedWrite runVar RunStats{rs_run = 2, rs_hadChanges = True, rs_staleCaps = 0}
    result <- timeout 2000000 (waitForFullSettle runVar 1)
    case result of
      Nothing -> assertFailure "waitForFullSettle didn't observe the later settle"
      Just rs -> do
        assertEqual 2 (rs_run rs)
        assertEqual 0 (rs_staleCaps rs)
 where
  forkDelayedWrite runVar stats =
    forkIO $ do
      threadDelay 50000
      atomically (writeTVar runVar (Some stats))
