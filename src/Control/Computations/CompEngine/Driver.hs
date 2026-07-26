module Control.Computations.CompEngine.Driver (
  RunStats (..),
  compDriver,
  compDriver',
  regSrc,
  regSink,
  waitForRunAtLeast,
  waitForFullSettle,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import Data.Time.Clock

-- | Summary of one iteration of the driver's run loop: which run number it
-- was, whether any source changes were found, and how many cached results
-- were stale going in.
data RunStats = RunStats
  { rs_run :: Int
  , rs_hadChanges :: Bool
  , rs_staleCaps :: Int
  }

-- | Drive a computation graph forward forever: register the flows given by
-- @withRegisteredFlows@ (typically a chain of 'regSrc'\/'regSink' calls
-- ending in your own action), wire up the graph with @wireComps@, evaluate
-- the root comp at @startVal@, and keep rerunning whatever's affected as
-- sources report changes. Returns only if @withRegisteredFlows@'s inner
-- action returns (e.g. because it was given a bounded action rather than one
-- that runs forever).
compDriver
  :: (IsCompParam p, IsCompResult r)
  => (CompFlowRegistry -> IO () -> IO ())
  -> CompWireM (Comp p r)
  -> p
  -> IO ()
compDriver withRegisteredFlows wireComps startVal = do
  runVar <- newTVarIO None
  compDriver' runVar withRegisteredFlows wireComps startVal

compDriver'
  :: (IsCompParam p, IsCompResult r)
  => TVar (Option RunStats)
  -> (CompFlowRegistry -> IO () -> IO ())
  -> CompWireM (Comp p r)
  -> p
  -> IO ()
compDriver' runVar withRegisteredFlows wireComps startVal = do
  reg <- newCompFlowRegistry
  withStateIf $ \stateIf -> withRegisteredFlows reg $ do
    let ifs =
          CompEngineIfs
            { ce_compFlowRegistry = reg
            , ce_stateIf = stateIf
            }
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = shouldStartNextRun stateIf reg runVar
            , rcif_emptyChangesMode = Block
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = (seconds 10)
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = garbageHandler reg
            }
    comps <- rootComps
    runCompEngine ifs comps rifs ()
 where
  runDeletes stateIf reg =
    do
      logNote ("Deleting leftovers from previous program run")
      forAllSinks_ reg (deleteDeadOutputs stateIf)
  shouldStartNextRun stateIf reg runVar nRun hadChanges nStaleCaps state =
    do
      when (nRun == 1) (runDeletes stateIf reg)
      let !stats = RunStats{rs_run = nRun, rs_hadChanges = hadChanges, rs_staleCaps = nStaleCaps}
      atomically $ writeTVar runVar (Some stats)
      pure (startNextRun, state)
  rootComps = (failInM . fmap snd) $
    runCompWireM $
      do
        c <- wireComps
        pure ([wrapCompAp (mkCompAp c startVal)])

-- | Register a source with a 'CompFlowRegistry', then run @action@. Written
-- to chain: @regSrc reg (regSrc reg action src2) src1@, or equivalently as
-- @\\action -> regSrc reg action src@ passed to 'compDriver'.
regSrc :: CompSrc s => CompFlowRegistry -> IO a -> s -> IO a
regSrc reg action src = do
  registerCompSrc reg src
  action

-- | Like 'regSrc', but for a sink.
regSink :: CompSink s => CompFlowRegistry -> IO a -> s -> IO a
regSink reg action sink = do
  registerCompSink reg sink
  action

{- | Blocks until the driver has posted a 'RunStats' whose 'rs_run' is at
 least @n@, and returns that 'RunStats' -- whichever one first satisfies the
 condition, not necessarily @rs_run == n@.

 This "at least" contract is easy to trip over: 'shouldStartNextRun' (see
 'compDriver'') posts run @k@'s stats -- describing run @k-1@'s leftover
 'rs_staleCaps' -- at the /top/ of the loop, before run @k@ has attempted
 its own blocking wait for changes. So by the time a caller's
 'waitForRunAtLeast' transaction actually gets scheduled, the driver may
 already have raced ahead and posted a later run than the caller expected.
 Callers must not assume the returned 'rs_run' equals @n@ -- in particular,
 never compute "the next run after this one" as @rs_run result + 1@ from a
 result obtained this way without first checking whether it is already
 settled ('rs_staleCaps' @== 0@); see 'waitForFullSettle', which does this
 correctly.
-}
waitForRunAtLeast :: TVar (Option RunStats) -> Int -> IO RunStats
waitForRunAtLeast runVar n = atomically $ do
  mrs <- readTVar runVar
  case mrs of
    Some rs | rs_run rs >= n -> pure rs
    _ -> retry

{- | Waits until propagation has genuinely finished, not merely until the run
 counter has ticked forward once. The driver (see 'compDriver'') processes
 at most 'Control.Computations.Utils.TimeSpan.seconds' 10 worth of stale
 caps per loop iteration before yielding back to check in -- so one
 incremental update's cascade can span *multiple* run numbers, each
 carrying a nonzero leftover 'rs_staleCaps' into the next.

 The queue is only certifiably empty once a freshly-observed run reports an
 *incoming* backlog ('rs_staleCaps') of zero -- i.e. the run immediately
 prior fully drained everything it saw with nothing carried over. This
 polls forward run by run (each an efficient STM 'retry', not a busy loop)
 until that happens, starting from @startRun@ (typically the caller's own
 most-recently-observed run number -- see 'waitForRunAtLeast's docs for why
 that must not be blindly incremented by callers themselves).
-}
waitForFullSettle :: TVar (Option RunStats) -> Int -> IO RunStats
waitForFullSettle runVar startRun = go startRun
 where
  go n = do
    rs <- waitForRunAtLeast runVar n
    if rs_staleCaps rs == 0
      then pure rs
      else go (rs_run rs + 1)
