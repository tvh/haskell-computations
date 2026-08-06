{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Enforces @docs\/benchmark-notes.md@'s "The ordering contract, which
 changed" section (Stage 15) -- the surviving guarantees of a
 'Control.Computations.CompEngine.Impl' engine once eval-side concurrency
 (the permit pool, promise table, and sliding fork window
 "TestCompEvalConcurrency" already exercises) is turned on. That section's
 own closing note is what this module exists to close out: "nothing
 currently tests the changed contract... restating it -- assert the
 multiset of entries plus the surviving happens-before edges, rather than a
 total order -- is open work." One test per surviving clause, named after
 the clause it pins:

 1. width 1 is byte-identical, evaluation order included;
 2. dedup yields one *value*, not merely one evaluation;
 3. the trace multiset is width-invariant;
 4. within one body, order is preserved at width 8;
 5. the leftmost-failing-*eval*-leaf guarantee (the existing coverage in
    "TestCompFlowConcurrency" is source-leaves only);
 6. every fork is joined before its batch returns, including on the
    exception path;
 7. 'Control.Computations.CompEngine.Types.CompMEnv'\'s @cme_deps@ stays
    per-evaluation at width 8.

 Deliberately a sibling module to "TestCompEvalConcurrency" (which pins the
 promise table\/permit pool\/sliding window's own internal mechanics) and to
 "TestCompFlowConcurrency" (which pins the analogous *source*-side
 contract) rather than an extension of either -- this module is entirely
 about the surviving cross-cutting guarantees once both are combined, not
 about either mechanism in isolation. It reuses their established patterns
 (a hand-built 'CompFlowRegistry' so 'setCompEvalConcurrency' can be called
 before 'initCompEngine' reads it, 'observingStateIf' for a trace, the
 @peekPromiseAwaits@ polling trick for a forced ordering) rather than
 importing them, since none of the sibling modules export anything beyond
 their own @htf_thisModulesTests@.

 __Determinism, not luck.__ Per this repo's own flake history (see
 "TestCompEvalConcurrency"'s module haddock and its test 1's own comment:
 one test in this exact area failed ~37-48% of standalone runs until it was
 rewritten to poll a real counter instead of sleeping), every test below
 that depends on a specific interleaving forces it with a genuine signal --
 an 'MVar' handshake, or 'Impl.peekPromiseAwaits' -- never a
 'Control.Concurrent.threadDelay'. A bounded 'System.Timeout.timeout' shows
 up in a few places purely as a deadlock backstop (loud, fast failure
 instead of hanging the whole suite if an assumption this module documents
 ever stops holding), never as the synchronisation mechanism itself -- each
 use says so at its call site.
-}
module Control.Computations.CompEngine.Tests.TestCompEvalOrderingContract (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CacheBehaviors
import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompEval
import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Impl (CompEngine, evalWithCompEngine, initCompEngine, peekPromiseAwaits)
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Tests.ObservingStateIf
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Fail
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types (Option (..))

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent (rtsSupportsBoundThreads, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, finally, throwIO, try)
import Control.Monad
import Data.Proxy
import qualified Data.HashSet as HashSet
import Data.IORef
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Timeout (timeout)
import Test.Framework

----------------------------------------------------------------------------
-- Shared plumbing: a fresh registry + rifs pair, mirroring both siblings'
-- own runMainComp/runOnce.
----------------------------------------------------------------------------

runMainComp :: CompFlowRegistry -> CompEngineStateIf IO -> Comp () () -> IO ()
runMainComp reg stateIf mainComp =
  runCompEngine ifs caps rifs ()
 where
  ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
  caps = [wrapCompAp (mkCompAp mainComp ())]
  rifs =
    RunCompEngineIf
      { rcif_shouldStartWithRun = \_ _ _ s -> pure (noNextRun, s)
      , rcif_emptyChangesMode = DontBlock
      , rcif_getTime = getCurrentTime
      , rcif_maxLoopRunTime = seconds 10
      , rcif_maxRunIterations = CompRunUnlimitedIterations
      , rcif_reportGarbage = \_ -> pure ()
      }

----------------------------------------------------------------------------
-- 1 & 3 & 4: shared ordering-trace fixture -- four independent eval leaves
-- (otc-worker(1..4)), each of which, within its OWN body, does a nested
-- eval (otc-inner) followed by a sink write, combined via 'traverse' (not
-- 'mapM_') into one 'CompReqCombined' batch of more than two leaves, so it
-- always takes the general (Prep-based) path regardless of width (see
-- Impl.hs's doSuspended -- the two-leaf fast path is excluded above width
-- 1 anyway, but four leaves rules that fast path out entirely, matching
-- test B.1/B.2 in "TestCompReqCombined"). No data source anywhere in this
-- fixture: the contract's surviving/lost clauses for *source* leaves are
-- already "TestCompFlowConcurrency"'s job; this fixture isolates the
-- eval+sink half.
----------------------------------------------------------------------------

data OtcWriteReq a where
  OtcWriteReq :: String -> OtcWriteReq ()

-- | Appends @"sink otc-sink <value>"@ to a shared trace on every write --
-- the sink-side counterpart of "TestCompReqCombined"'s own @TraceSink@,
-- redefined locally since that module exports nothing beyond its own
-- tests.
data OtcSink = OtcSink
  { otc_name :: T.Text
  , otc_traceRef :: IORef [String]
  }
  deriving (Typeable)

instance CompSink OtcSink where
  type CompSinkReq OtcSink = OtcWriteReq
  type CompSinkOut OtcSink = String
  compSinkInstanceId = CompSinkInstanceId . otc_name
  compSinkExecute sink (OtcWriteReq val) =
    do
      let entry = "sink " ++ T.unpack (otc_name sink) ++ " " ++ val
      atomicModifyIORef' (otc_traceRef sink) (\xs -> (xs ++ [entry], ()))
      pure (HashSet.singleton val, Ok ())
  compSinkDeleteOutputs _ _ = pure ()
  compSinkListExistingOutputs _ = None

otcSinkId :: TypedCompSinkId OtcSink
otcSinkId = unsafeMkTypedCompSinkId (Proxy @OtcSink) "otc-sink"

otcInnerCompDef :: CompDef Int Int
otcInnerCompDef = defineComp "otc-inner" fullCaching $ \p -> pure p

-- | Each worker's own body: a nested eval (otc-inner(p)), then a sink
-- write tagged with @p@ -- two ordered engine-visible events per body,
-- which is what makes test 4 ("within one body, order is preserved")
-- meaningful: a single worker already emits more than one trace entry to
-- check the relative order of.
otcWorkerCompDef :: Comp Int Int -> TypedCompSinkId OtcSink -> CompDef Int Int
otcWorkerCompDef inner sinkId =
  defineComp "otc-worker" fullCaching $ \p ->
    do
      Just v <- evalComp inner p
      compSinkReq sinkId (OtcWriteReq ("w" ++ show p ++ "-done"))
      pure v

otcMainCompDef :: Comp Int Int -> CompDef () ()
otcMainCompDef worker =
  defineComp "otc-main" inMemoryShowCaching $ \() ->
    void (traverse (evalCompOrFail worker) [1 .. 4 :: Int])

-- | Run the shared fixture once, returning the trace it produced.
-- 'Nothing' never calls 'setCompEvalConcurrency' at all (the untouched
-- default -- see 'Control.Computations.CompEngine.Impl.initCompEngine'\'s
-- own haddock on why that reads as width 1); @'Just' n@ sets the width to
-- @n@ before the engine is built (required timing -- see
-- "TestCompEvalConcurrency"'s own module haddock).
runOrderingTrace :: Maybe Int -> IO [String]
runOrderingTrace mWidth =
  do
    traceRef <- newIORef []
    let sink = OtcSink{otc_name = "otc-sink", otc_traceRef = traceRef}
    reg <- newCompFlowRegistry
    forM_ mWidth $ \w -> setCompEvalConcurrency reg (mkCompEvalConcurrency w)
    registerCompSink reg sink
    (rawStateIf, closeSif) <- initStateIf True
    let stateIf =
          observingStateIf
            (\cap -> atomicModifyIORef' traceRef (\xs -> (xs ++ ["eval " ++ show cap], ())))
            rawStateIf
    (_compMap, mainComp) <- failInM $ runCompWireM compDefs
    runMainComp reg stateIf mainComp `finally` closeSif
    readIORef traceRef
 where
  compDefs =
    do
      inner <- wireComp otcInnerCompDef
      worker <- wireComp (otcWorkerCompDef inner otcSinkId)
      wireComp (otcMainCompDef worker)

----------------------------------------------------------------------------
-- 1: width 1 is byte-identical, evaluation order included -- the
-- strongest claim in the contract, and the one that keeps "a default
-- (width-1) engine produces bit-identical behaviour to the unmodified
-- engine" (see Impl.hs's own 'CompEngine' haddock on 'ce_par') honest at
-- the trace level, not just at the allocation-counter level that comment
-- talks about. If a future change let, say, otc-inner's nested eval float
-- relative to otc-worker's own capEvaluationStarted call even at width 1
-- (nothing in the contract permits that -- reordering is only licensed
-- "above width 1"), this fails immediately: both the untouched-default run
-- and the explicit-width-1 run are checked against the very same frozen
-- literal, so either one drifting is caught.
----------------------------------------------------------------------------

-- | Frozen by construction, exactly like "TestCompReqCombined"'s own
-- golden trace: obtained by actually running the fixture once and reading
-- off what it produced, not hand-derived. Do not edit -- a change here
-- should only ever happen alongside a deliberate, understood change to
-- width-1 evaluation order itself.
goldenOrderingTrace :: [String]
goldenOrderingTrace =
  [ "eval otc-main(())"
  , "eval otc-worker(1)"
  , "eval otc-inner(1)"
  , "sink otc-sink w1-done"
  , "eval otc-worker(2)"
  , "eval otc-inner(2)"
  , "sink otc-sink w2-done"
  , "eval otc-worker(3)"
  , "eval otc-inner(3)"
  , "sink otc-sink w3-done"
  , "eval otc-worker(4)"
  , "eval otc-inner(4)"
  , "sink otc-sink w4-done"
  ]

test_widthOneTraceByteIdenticalIncludingEvalOrder :: IO ()
test_widthOneTraceByteIdenticalIncludingEvalOrder =
  do
    traceDefault <- runOrderingTrace Nothing
    traceExplicitWidth1 <- runOrderingTrace (Just 1)
    assertEqual goldenOrderingTrace traceDefault
    assertEqual goldenOrderingTrace traceExplicitWidth1

----------------------------------------------------------------------------
-- 3: the trace multiset is width-invariant -- concurrency may reorder
-- *which* entries interleave with which (see test 1's own contrast), but
-- it must never duplicate or drop work. Sorting erases order and leaves
-- only the multiset, so this catches exactly the two failure modes the
-- contract's "No longer guaranteed above width 1" paragraph does NOT
-- excuse: a leaf evaluated twice (a dedup regression) or a leaf silently
-- skipped (a fork whose result never got merged back in) would each change
-- the sorted trace's *length* or *contents*, not just its order, at some
-- width here even if it happened to look fine at another.
----------------------------------------------------------------------------

test_traceMultisetWidthInvariantAcross1And4And16 :: IO ()
test_traceMultisetWidthInvariantAcross1And4And16 =
  do
    trace1 <- runOrderingTrace (Just 1)
    trace4 <- runOrderingTrace (Just 4)
    trace16 <- runOrderingTrace (Just 16)
    assertEqual (L.sort trace1) (L.sort trace4)
    assertEqual (L.sort trace1) (L.sort trace16)

----------------------------------------------------------------------------
-- 4: within one body, order is preserved at width 8 -- the contract's own
-- explicit carve-out ("Within one body, order is preserved") from the
-- paragraph that otherwise licenses reordering above width 1. otc-worker(3)
-- emits two entries from inside its own body (its nested "eval
-- otc-inner(3)", then "sink otc-sink w3-done"), plus its own
-- capEvaluationStarted entry immediately precedes both -- filtering the
-- width-8 trace down to just those three and comparing to the fixed
-- 3-entry order pins that even while three OTHER workers' bodies are
-- interleaving all around it (their own eval/sink entries are free to
-- land anywhere else in the trace -- see test 3), worker(3)'s own
-- internal happens-before edges never get scrambled by whichever thread
-- ends up running it.
----------------------------------------------------------------------------

test_withinOneBodyOrderPreservedAtEvalWidth8 :: IO ()
test_withinOneBodyOrderPreservedAtEvalWidth8 =
  do
    trace <- runOrderingTrace (Just 8)
    let expected = ["eval otc-worker(3)", "eval otc-inner(3)", "sink otc-sink w3-done"]
        mine = filter (`elem` expected) trace
    assertEqual expected mine

----------------------------------------------------------------------------
-- 2: dedup yields one *value*, not merely one evaluation. Reworks
-- "TestCompEvalConcurrency"'s own
-- @test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8@
-- (which only counts capEvaluationStarted calls) to also check *value*
-- identity: otc-hashcache-leaf's body reads a shared, mutating counter via
-- a CompSrc (the only way a comp body can observe external mutable state
-- at all -- CompM has no liftIO), so a second, spurious evaluation would
-- necessarily read a *different* counter value, not coincidentally the
-- same one. Referencing the cap twice and asserting both references
-- return the identical Int -- and that the counter's own execution log
-- shows exactly one read tagged for this cap's param -- catches a
-- regression where evaluation count stays right (still 1, if some other
-- bookkeeping happened to dedupe the *count*) but two separate reads to
-- the counter still occurred and only one of their results got kept
-- (impossible under today's engine, but not something the count alone
-- would ever catch).
----------------------------------------------------------------------------

data CounterReq a where
  ReadCounter :: String -> CounterReq Int

-- | A mutating counter, exposed as a 'CompSrc' (the only channel a comp
-- body has into IO). Every read bumps the shared 'IORef' and appends
-- @(tag, newValue)@ to a log -- @tag@ is the requesting cap's own param
-- (as text), so the log can be filtered back down to "how many times did
-- THIS cap's identity actually read the counter" without needing any
-- engine-level instrumentation.
data CounterSrc = CounterSrc
  { cs_counter :: IORef Int
  , cs_log :: IORef [(String, Int)]
  }
  deriving (Typeable)

instance CompSrc CounterSrc where
  type CompSrcReq CounterSrc = CounterReq
  type CompSrcKey CounterSrc = String
  type CompSrcVer CounterSrc = Int
  compSrcInstanceId _ = CompSrcInstanceId "otc-counter"
  compSrcExecute src (ReadCounter tag) =
    do
      v <- atomicModifyIORef' (cs_counter src) (\n -> (n + 1, n + 1))
      atomicModifyIORef' (cs_log src) (\xs -> (xs ++ [(tag, v)], ()))
      pure (HashSet.singleton (Dep tag v), Ok v)
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry

otcCounterLeafCompDef :: TypedCompSrcId CounterSrc -> CompDef Int Int
otcCounterLeafCompDef srcId =
  defineComp "otc-hashcache-leaf" hashCaching $ \p ->
    compSrcReq srcId (ReadCounter (show p))

-- | Three leaves combined via '<*>' -- the same shape
-- "TestCompEvalConcurrency"'s own hashCaching test uses, and for the same
-- reason: three leaves always take the general (Prep-based) path
-- regardless of width, unlike a plain two-leaf @f \<$\> a \<*\> b@ (see
-- Impl.hs's doSuspended -- the two-leaf fast path only excludes itself
-- above width 1, and this needs to work at width 8 regardless of that
-- detail).
otcCounterMainCompDef :: Comp Int Int -> CompDef () (Int, Int, Int)
otcCounterMainCompDef leaf =
  defineComp "otc-hashcache-main" inMemoryShowCaching $ \() ->
    (,,)
      <$> evalCompOrFail leaf 7
      <*> evalCompOrFail leaf 7
      <*> evalCompOrFail leaf 8

-- | Force @COMP_ENGINE_LOCK_STATS@ on for the duration of @act@ -- the only
-- way 'Control.Computations.CompEngine.Impl.initCompEngine' builds an
-- 'EngineDiag' at all (see 'peekPromiseAwaits'\'s own haddock), restoring
-- whatever the environment held before. Copied from
-- "TestCompEvalConcurrency" rather than imported -- that module exports
-- nothing beyond its own tests.
forceCompEngineLockStats :: IO a -> IO a
forceCompEngineLockStats act =
  bracket
    (lookupEnv envVar <* setEnv envVar "1")
    (maybe (unsetEnv envVar) (setEnv envVar))
    (const act)
 where
  envVar = "COMP_ENGINE_LOCK_STATS"

{- | Block the promise table's OWNER (whichever of otc-hashcache-leaf(7)'s
 two references wins the claim) until 'peekPromiseAwaits' reports the
 second reference has actually joined -- forcing the outcome under test
 instead of hoping the second reference's fork wins its race before the
 first reference's evaluation (here, a counter read plus a return -- fast)
 completes and releases its claim. Identical trick to
 "TestCompEvalConcurrency"'s own @awaitPromiseJoinThenProceed@; see that
 function's haddock for the full argument (in short: on unmodified engine
 code this exact race was measured to fail 37-48% of standalone runs
 before it was rewritten to poll this counter instead of sleeping). The
 30s timeout is a deadlock backstop only, not the synchronisation
 mechanism -- at width 8 with 7 permits and only one other eligible leaf
 in this batch, the second reference's own collect-time fork attempt
 should essentially always win a permit and start well within it.
-}
awaitPromiseJoinThenProceed :: CompEngine -> IO ()
awaitPromiseJoinThenProceed engine =
  timeout (30 * 1000 * 1000) pollUntilJoined >>= \case
    Just () -> pure ()
    Nothing ->
      error
        "test_dedupYieldsOneValueNotJustOneEvaluationAtEvalWidth8: timed out after \
        \30s waiting for ed_promiseAwaits to reach 1 -- the second reference to \
        \otc-hashcache-leaf(7) never joined the first's promise"
 where
  pollUntilJoined = do
    n <- peekPromiseAwaits engine
    if n >= 1
      then pure ()
      else threadDelay 200 >> pollUntilJoined

test_dedupYieldsOneValueNotJustOneEvaluationAtEvalWidth8 :: IO ()
test_dedupYieldsOneValueNotJustOneEvaluationAtEvalWidth8 =
  forceCompEngineLockStats $
    do
      counterRef <- newIORef 0
      logRef <- newIORef []
      let src = CounterSrc counterRef logRef
      engineRef <- newIORef Nothing
      let onEval :: forall a. CompAp a -> IO ()
          onEval cap =
            when (show cap == "otc-hashcache-leaf(7)") $
              readIORef engineRef >>= \case
                Nothing ->
                  error
                    "test_dedupYieldsOneValueNotJustOneEvaluationAtEvalWidth8: \
                    \capEvaluationStarted fired before initCompEngine returned -- \
                    \should be impossible"
                Just engine -> awaitPromiseJoinThenProceed engine
          wrapStateIf = observingStateIf onEval
      reg <- newCompFlowRegistry
      setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
      registerCompSrc reg src
      (rawStateIf, closeSif) <- initStateIf True
      let stateIf = wrapStateIf rawStateIf
      (_compMap, mainComp) <-
        failInM $
          runCompWireM $
            do
              leaf <- wireComp (otcCounterLeafCompDef (typedCompSrcIdOf src))
              wireComp (otcCounterMainCompDef leaf)
      engine <-
        initCompEngine CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
      writeIORef engineRef (Just engine)
      Just (v7a, v7b, _v8) <-
        evalWithCompEngine engine (mkCompAp mainComp ()) `finally` closeSif
      finalLog <- readIORef logRef
      let sevenEntries = filter ((== "7") . fst) finalLog
      -- Exactly one counter read attributable to param 7 -- two would mean
      -- otc-hashcache-leaf(7) was genuinely re-evaluated, not just
      -- re-counted.
      assertEqual 1 (length sevenEntries)
      -- Both references to leaf(7) observed the identical Int, and it is
      -- the one value the log recorded for it -- not merely "happened to
      -- be equal", but literally the first (only) evaluation's own result.
      assertEqual v7a v7b
      assertEqual (Just v7a) (fmap snd (L.find ((== "7") . fst) finalLog))

----------------------------------------------------------------------------
-- 5 & 6: shared fixture -- a generic single-request CompSrc whose one
-- action is an arbitrary IO closure, exactly "TestCompFlowConcurrency"'s
-- own @ScriptedSrc@ pattern ("every test... builds its own instance(s), so
-- the interesting behaviour lives entirely in [the] action closures"),
-- reimplemented locally (that module exports nothing beyond its own
-- tests) and used here to make an EVAL leaf's own body fail via a genuine
-- IO exception -- the only way to get a real 'Control.Exception.throwIO'
-- (as opposed to an ordinary 'Control.Computations.Utils.Fail.Fail' value)
-- out of a comp body, which has no liftIO.
----------------------------------------------------------------------------

data ActionReq a where
  ActionReq :: ActionReq ()

data ActionSrc = ActionSrc
  { act_name :: T.Text
  , act_action :: IO ()
  }
  deriving (Typeable)

instance CompSrc ActionSrc where
  type CompSrcReq ActionSrc = ActionReq
  type CompSrcKey ActionSrc = Int
  type CompSrcVer ActionSrc = Int
  compSrcInstanceId = CompSrcInstanceId . act_name
  compSrcExecute src ActionReq =
    do
      act_action src
      pure (HashSet.singleton (Dep 0 (0 :: Int)), Ok ())
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry

actionLeafCompDef :: String -> TypedCompSrcId ActionSrc -> CompDef () ()
actionLeafCompDef name srcId =
  defineComp name inMemoryShowCaching $ \() -> compSrcReq srcId ActionReq

----------------------------------------------------------------------------
-- 5: the leftmost-failing-*eval*-leaf guarantee. "TestCompFlowConcurrency"'s
-- @test_leftmostFailingSourceLeafExceptionEscapesAtWidth8@ already pins
-- this for source leaves; nothing pinned it for EVAL leaves, where the
-- mechanism is different (prepEvalLeaf's own fork/join, not a SrcGroup) --
-- see Impl.hs's prepEvalLeaf haddock, "Only *starts* move; *joins* do
-- not", for the argument this test exercises directly.
--
-- Two eval leaves, left and right, each failing via a genuine 'throwIO'
-- from inside their own body's compSrcReq. RIGHT fails immediately; LEFT
-- is held (via a real 'MVar' handshake, not a delay) until AFTER right has
-- already signalled its own failure, so right's exception is always fully
-- resolved before left's is even thrown. A naive "whichever exception
-- reaches the top first wins" implementation would therefore report
-- RIGHT's message; this asserts LEFT's message escapes instead, exactly
-- matching compMAp's left-error bias at the Fail level (see its own
-- haddock in Types.hs) and prepSrcLeaf's identical guarantee on the source
-- side.
--
-- LEFT is always this batch's first eval leaf, so per prepEvalLeaf's own
-- documented invariant ("Never forks the batch's first eval leaf") it
-- always runs inline on the calling/engine thread; RIGHT is the second
-- leaf, eligible to fork. This design depends on RIGHT actually forking --
-- if it instead ran inline too, LEFT's own takeMVar would block forever
-- with nothing left to fill it (Prep's strictly left-to-right join order
-- means LEFT's own inline action runs before RIGHT's would even be
-- reached) -- hence the 'rtsSupportsBoundThreads' guard (this repo's own
-- test stanza always builds @-threaded@, so this is never actually
-- skipped) and the bounded 'timeout' backstop below, which turns "the fork
-- assumption stopped holding" into a fast, loud failure instead of a
-- hung test suite.
----------------------------------------------------------------------------

test_leftmostFailingEvalLeafExceptionEscapesAtWidth8 :: IO ()
test_leftmostFailingEvalLeafExceptionEscapesAtWidth8 =
  when rtsSupportsBoundThreads $
    do
      rightFailed <- newEmptyMVar
      let leftAction = takeMVar rightFailed >> throwIO (userError "left eval boom")
          rightAction = putMVar rightFailed () >> throwIO (userError "right eval boom")
          leftSrc = ActionSrc "eval-boom-left" leftAction
          rightSrc = ActionSrc "eval-boom-right" rightAction
      reg <- newCompFlowRegistry
      -- This is a plain two-leaf `(,) <$> a <*> b` batch, and doSuspended's
      -- own two-leaf fast path (Impl.hs) is gated on whether parallel eval
      -- is enabled at all (@ce_par@ 'Nothing') -- see
      -- "TestCompFlowConcurrency"'s own test 6 comment. Leaving eval
      -- concurrency at its default of 1 would route this batch through the
      -- old sequential fast path instead of prepEvalLeaf, where nothing
      -- ever forks and LEFT's own takeMVar would block forever (exactly
      -- what this design's own comment above warns about).
      setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
      registerCompSrc reg leftSrc
      registerCompSrc reg rightSrc
      (_compMap, mainComp) <-
        failInM $
          runCompWireM $
            do
              leftLeaf <- wireComp (actionLeafCompDef "eval-boom-left-leaf" (typedCompSrcIdOf leftSrc))
              rightLeaf <- wireComp (actionLeafCompDef "eval-boom-right-leaf" (typedCompSrcIdOf rightSrc))
              wireComp $
                defineComp "eval-boom-main" inMemoryShowCaching $ \() ->
                  void $ (,) <$> evalCompOrFail leftLeaf () <*> evalCompOrFail rightLeaf ()
      (rawStateIf, closeSif) <- initStateIf True
      let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = rawStateIf}
          caps = [wrapCompAp (mkCompAp mainComp ())]
          rifs =
            RunCompEngineIf
              { rcif_shouldStartWithRun = \_ _ _ s -> pure (noNextRun, s)
              , rcif_emptyChangesMode = DontBlock
              , rcif_getTime = getCurrentTime
              , rcif_maxLoopRunTime = seconds 10
              , rcif_maxRunIterations = CompRunUnlimitedIterations
              , rcif_reportGarbage = \_ -> pure ()
              }
      mOutcome <-
        timeout (30 * 1000 * 1000) $
          try (runCompEngine ifs caps rifs () `finally` closeSif)
      case mOutcome of
        Nothing ->
          error
            "test_leftmostFailingEvalLeafExceptionEscapesAtWidth8: timed out after \
            \30s -- eval-boom-right-leaf likely never forked (see this test's own \
            \comment on why that would deadlock eval-boom-left-leaf's own MVar wait)"
        Just (Left (e :: IOException)) ->
          assertBool ("left eval boom" `L.isInfixOf` show e)
        Just (Right ()) ->
          assertFailure "expected the batch to throw, but it returned normally"

----------------------------------------------------------------------------
-- 6: every fork is joined before its batch returns, including on the
-- exception path -- what keeps allCompSrcChanges from ever racing a
-- running compSrcExecuteBatch (see Impl.hs's doSuspended, the three-reason
-- comment on the general path's own 'cancelAllTracked' call). Reuses test
-- 5's left/right failing pair unchanged, plus a third STRAY leaf whose only
-- job is to still be genuinely in flight -- blocked, not finished -- at the
-- exact moment the batch fails, so this test can tell "torn down by
-- cancelAllTracked" apart from "coincidentally finished before anyone
-- needed to tear it down" (test 5's own left/right pair can't distinguish
-- those: right fails fast enough that it's normally already done by the
-- time left throws, fork or no fork).
--
-- STRAY announces it has started (and is now blocked) via its own MVar;
-- LEFT waits for both right's failure signal AND stray's start signal
-- before throwing, so by construction stray is DEFINITELY still parked
-- inside its own takeMVar the instant the whole batch fails. Every one of
-- the three leaves' actions is wrapped in a started\/finished counter pair
-- (bumped via a genuine 'Control.Exception.finally', which fires on
-- success, a synchronous throw, AND an asynchronous cancellation alike --
-- this is exactly why 'Async.cancel' is documented to block until the
-- cancelled thread's own cleanup has actually run). "finished count equals
-- started count once the run has returned" is therefore a direct, external
-- read of "no thread this batch forked is still unwinding (or worse, still
-- running) after the call that owns it has already returned" -- if
-- cancelAllTracked were ever skipped on the exception path, stray's
-- finished bump would never fire (it would still be blocked, forever,
-- on a thread nobody is waiting on anymore) and this would catch that as
-- startedCount /= finishedCount, not merely as a leaked thread nothing
-- ever notices.
----------------------------------------------------------------------------

test_everyForkJoinedBeforeBatchReturnsOnExceptionPathAtWidth8 :: IO ()
test_everyForkJoinedBeforeBatchReturnsOnExceptionPathAtWidth8 =
  when rtsSupportsBoundThreads $
    do
      rightFailed <- newEmptyMVar
      strayStarted <- newEmptyMVar
      strayUnblock <- newEmptyMVar
      startedRef <- newIORef (0 :: Int)
      finishedRef <- newIORef (0 :: Int)
      strayFinishedNormally <- newIORef False
      let counted act =
            (atomicModifyIORef' startedRef (\n -> (n + 1, ())) >> act)
              `finally` atomicModifyIORef' finishedRef (\n -> (n + 1, ()))
          leftAction =
            counted $
              takeMVar rightFailed >> takeMVar strayStarted >> throwIO (userError "left eval boom")
          rightAction =
            counted $
              putMVar rightFailed () >> throwIO (userError "right eval boom")
          strayAction =
            counted $
              do
                putMVar strayStarted ()
                -- Never filled by this test -- stray is only ever unblocked by
                -- Async.cancel delivering it an asynchronous exception, which
                -- 'counted's own 'finally' still observes (see this section's
                -- own comment). A bounded backstop MVar (never put to under
                -- normal operation) rather than a bare 'forever' loop, purely
                -- so a regression that somehow left this thread alive would
                -- show up as a hung takeMVar instead of a busy-spinning one.
                _ <- takeMVar strayUnblock
                writeIORef strayFinishedNormally True
          leftSrc = ActionSrc "join-boom-left" leftAction
          rightSrc = ActionSrc "join-boom-right" rightAction
          straySrc = ActionSrc "join-boom-stray" strayAction
      reg <- newCompFlowRegistry
      setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
      registerCompSrc reg leftSrc
      registerCompSrc reg rightSrc
      registerCompSrc reg straySrc
      (_compMap, mainComp) <-
        failInM $
          runCompWireM $
            do
              leftLeaf <- wireComp (actionLeafCompDef "join-boom-left-leaf" (typedCompSrcIdOf leftSrc))
              rightLeaf <- wireComp (actionLeafCompDef "join-boom-right-leaf" (typedCompSrcIdOf rightSrc))
              strayLeaf <- wireComp (actionLeafCompDef "join-boom-stray-leaf" (typedCompSrcIdOf straySrc))
              wireComp $
                defineComp "join-boom-main" inMemoryShowCaching $ \() ->
                  void $
                    (,,)
                      <$> evalCompOrFail leftLeaf ()
                      <*> evalCompOrFail rightLeaf ()
                      <*> evalCompOrFail strayLeaf ()
      (rawStateIf, closeSif) <- initStateIf True
      let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = rawStateIf}
          caps = [wrapCompAp (mkCompAp mainComp ())]
          rifs =
            RunCompEngineIf
              { rcif_shouldStartWithRun = \_ _ _ s -> pure (noNextRun, s)
              , rcif_emptyChangesMode = DontBlock
              , rcif_getTime = getCurrentTime
              , rcif_maxLoopRunTime = seconds 10
              , rcif_maxRunIterations = CompRunUnlimitedIterations
              , rcif_reportGarbage = \_ -> pure ()
              }
      mOutcome <-
        timeout (30 * 1000 * 1000) $
          try (runCompEngine ifs caps rifs () `finally` closeSif)
      case mOutcome of
        Nothing ->
          error
            "test_everyForkJoinedBeforeBatchReturnsOnExceptionPathAtWidth8: timed \
            \out after 30s -- join-boom-right-leaf/join-boom-stray-leaf likely \
            \never forked (see this test's own comment)"
        Just (Left (e :: IOException)) -> assertBool ("left eval boom" `L.isInfixOf` show e)
        Just (Right ()) -> assertFailure "expected the batch to throw, but it returned normally"
      -- The call above has already returned: every counted leaf's action
      -- must therefore have reached its own 'finally', stray included --
      -- proving stray was actually torn down (cancelled and unwound), not
      -- merely abandoned on some thread nobody is accounting for anymore.
      started <- readIORef startedRef
      finished <- readIORef finishedRef
      assertEqual started finished
      assertBool (started >= 3)
      -- Stray's own post-block line must never have run: reaching it would
      -- mean the takeMVar returned normally, which only 'strayUnblock'
      -- (never put to) could cause -- so this only trips if stray was
      -- somehow left running past this test's own lifetime instead of
      -- being genuinely cancelled.
      strayNormal <- readIORef strayFinishedNormally
      assertBool (not strayNormal)

----------------------------------------------------------------------------
-- 7: cme_deps stays per-evaluation at width 8 -- 'CompMEnv'\'s dependency
-- accumulator is a fresh 'IORef' allocated per call to 'evalCompAp'
-- (Impl.hs), so nothing about forking should ever let one concurrently-
-- running body's dependency reads leak into another's. Two eval leaves,
-- each reading a DIFFERENT key from the same source, forked concurrently
-- via a real rendezvous (so this genuinely exercises two bodies mutating
-- state at the same time, not just "happened to run one after another");
-- mutating exactly one key afterwards and checking that only its own
-- dependent cap reruns is what a leaked/shared depsRef would break --
-- with a shared accumulator, keyA-leaf's own dependency record would pick
-- up keyB's read too (or vice versa, depending on which body's writes
-- landed last), and bumping keyB alone would incorrectly also invalidate
-- the keyA leaf.
--
-- The rendezvous (a shared arrival counter, incremented then awaited via
-- STM 'retry' -- never a sleep) only ever needs to actually block on the
-- FIRST run, where both leaves are genuinely evaluated for the first time;
-- once the counter has reached 2 it stays there, so the later, single-key
-- rerun this test triggers (only keyB-leaf reruns, never both) passes
-- straight through without waiting on a second arrival that would never
-- come.
----------------------------------------------------------------------------

data DepsReq a where
  ReadDepsKey :: String -> DepsReq Int

data DepsSrc = DepsSrc
  { dep_verA :: TVar Int
  , dep_verB :: TVar Int
  , dep_changes :: TVar (HashSet.HashSet (Dep String Int))
  , dep_arrived :: TVar Int
  }

-- | Increment the shared arrival counter, then block (via STM 'retry',
-- watched on the very 'TVar' just written, not a sleep) until it reaches
-- 2. See this section's own header comment for why a later, single-arrival
-- call passes straight through once the counter has already reached 2 --
-- exactly the "only blocks on the first run" behaviour this fixture needs.
-- The 30s 'timeout' is a deadlock backstop only: at width 8 with 7 permits
-- and a single other eligible leaf, the second leaf's own fork should
-- essentially always win a permit and reach this rendezvous quickly.
depsRendezvous :: TVar Int -> IO ()
depsRendezvous arrived =
  do
    atomically (modifyTVar' arrived (+ 1))
    mDone <- timeout (30 * 1000 * 1000) (atomically (readTVar arrived >>= \n -> unless (n >= 2) retry))
    case mDone of
      Just () -> pure ()
      Nothing ->
        error
          "depsRendezvous: timed out after 30s -- deps-leaf-a/deps-leaf-b likely \
          \never both forked concurrently (see this section's own comment)"

instance CompSrc DepsSrc where
  type CompSrcReq DepsSrc = DepsReq
  type CompSrcKey DepsSrc = String
  type CompSrcVer DepsSrc = Int
  compSrcInstanceId _ = CompSrcInstanceId "otc-deps-src"
  compSrcExecute src (ReadDepsKey key) =
    do
      depsRendezvous (dep_arrived src)
      v <- readTVarIO (if key == "keyA" then dep_verA src else dep_verB src)
      pure (HashSet.singleton (Dep key v), Ok v)
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges src =
    do
      changes <- readTVar (dep_changes src)
      writeTVar (dep_changes src) HashSet.empty
      if HashSet.null changes then retry else pure changes

initDepsSrc :: IO DepsSrc
initDepsSrc = DepsSrc <$> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO HashSet.empty <*> newTVarIO 0

-- | Bump keyB's version alone and record it as a change -- mirrors
-- "TestCompReqCombined"'s own @bumpB@, so the driver loop's
-- @allCompSrcChanges@ wakes and marks exactly the caps that read keyB
-- stale.
bumpDepsKeyB :: DepsSrc -> Int -> IO ()
bumpDepsKeyB src v =
  atomically $
    do
      writeTVar (dep_verB src) v
      modifyTVar' (dep_changes src) (HashSet.insert (Dep "keyB" v))

depsLeafCompDef :: String -> String -> TypedCompSrcId DepsSrc -> CompDef () Int
depsLeafCompDef name key srcId =
  defineComp name fullCaching $ \() -> compSrcReq srcId (ReadDepsKey key)

depsMainCompDef :: Comp () Int -> Comp () Int -> CompDef () (Int, Int)
depsMainCompDef leafA leafB =
  defineComp "deps-main" inMemoryShowCaching $ \() ->
    (,) <$> evalCompOrFail leafA () <*> evalCompOrFail leafB ()

test_depsStayPerEvaluationAtEvalWidth8 :: IO ()
test_depsStayPerEvaluationAtEvalWidth8 =
  when rtsSupportsBoundThreads $
    do
      src <- initDepsSrc
      reg <- newCompFlowRegistry
      -- See test 5's own comment on why a plain two-leaf `(,) <$> a <*> b`
      -- batch (deps-main's own shape) needs eval concurrency raised to
      -- actually reach prepEvalLeaf instead of doSuspended's old sequential
      -- fast path.
      setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
      registerCompSrc reg src
      countsRef <- newIORef []
      (rawStateIf, closeSif) <- initStateIf True
      let stateIf =
            observingStateIf
              (\cap -> atomicModifyIORef' countsRef (\xs -> (show cap : xs, ())))
              rawStateIf
      (_compMap, mainComp) <-
        failInM $
          runCompWireM $
            do
              leafA <- wireComp (depsLeafCompDef "deps-leaf-a" "keyA" (typedCompSrcIdOf src))
              leafB <- wireComp (depsLeafCompDef "deps-leaf-b" "keyB" (typedCompSrcIdOf src))
              wireComp (depsMainCompDef leafA leafB)
      let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
          caps = [wrapCompAp (mkCompAp mainComp ())]
          -- run 1: initial settle; run 2: bump keyB alone right before the
          -- driver checks for changes; run 3: stop. Exactly
          -- "TestCompReqCombined"'s own B.3 shape.
          shouldStartNextRun :: Int -> Bool -> Int -> () -> IO (NextRun, ())
          shouldStartNextRun 1 _ _ () = pure (startNextRun, ())
          shouldStartNextRun 2 _ _ () = bumpDepsKeyB src 1 >> pure (startNextRun, ())
          shouldStartNextRun _ _ _ () = pure (noNextRun, ())
          rifs =
            RunCompEngineIf
              { rcif_shouldStartWithRun = shouldStartNextRun
              , rcif_emptyChangesMode = DontBlock
              , rcif_getTime = getCurrentTime
              , rcif_maxLoopRunTime = seconds 10
              , rcif_maxRunIterations = CompRunUnlimitedIterations
              , rcif_reportGarbage = \_ -> pure ()
              }
      timeout (30 * 1000 * 1000) (runCompEngine ifs caps rifs () `finally` closeSif) >>= \case
        Just () -> pure ()
        Nothing ->
          error
            "test_depsStayPerEvaluationAtEvalWidth8: timed out after 30s -- see \
            \depsRendezvous's own comment"
      allEvals <- readIORef countsRef
      let countOf name = length (filter (== name) allEvals)
      -- deps-leaf-a never depended on keyB, so bumping keyB alone must
      -- leave it at exactly its one initial evaluation -- a shared/leaked
      -- cme_deps would instead have recorded a (spurious) dependency on
      -- keyB for deps-leaf-a too, making this 2.
      assertEqual 1 (countOf "deps-leaf-a(())")
      -- deps-leaf-b DOES depend on keyB, so it must have rerun exactly
      -- once more after the bump.
      assertEqual 2 (countOf "deps-leaf-b(())")
