{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Regression guards for 'CompReqCombined' (batched applicative requests,
 built whenever a comp body combines suspended actions via @\<*\>@ -- see
 'Control.Computations.CompEngine.Types.compMAp'), written and passing
 against __today's unmodified engine__. That ordering is the whole point:
 these pin down three invariants ahead of upcoming work on batched
 applicative requests, so a regression shows up as a failing test rather
 than a silent behavior change. A test written after the change it's meant
 to catch proves nothing.
-}
module Control.Computations.CompEngine.Tests.TestCompReqCombined (htf_thisModulesTests) where

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
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Tests.ObservingStateIf
import Control.Computations.CompEngine.Tests.TestHelper
import Control.Computations.CompEngine.Types
import Control.Computations.FlowImpls.HashMapFlow
import Control.Computations.Utils.Fail
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IORef
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import Test.Framework

----------------------------------------------------------------------------
-- B.1: a wide applicative batch naming the same few caps many times must
-- still evaluate each distinct cap exactly once.
----------------------------------------------------------------------------

b1LeafCompDef :: CompDef Int Int
b1LeafCompDef = defineComp "b1-leaf" fullCaching $ \p -> pure p

-- | 'traverse' (not 'mapM_') is essential: it goes through 'Applicative'\'s
-- @\<*\>@, so all 1000 calls land in one 'CompReqCombined' batch rather than
-- 1000 sequential monadic steps -- exactly the shape this test needs to
-- guard.
b1MainCompDef :: Comp Int Int -> CompDef () ()
b1MainCompDef leaf =
  defineComp "b1-main" inMemoryShowCaching $ \() ->
    void (traverse (\i -> evalCompOrFail leaf (i `mod` 10)) [0 .. 999 :: Int])

test_wideApplicativeBatchEvaluatesEachDistinctCapOnce :: IO ()
test_wideApplicativeBatchEvaluatesEachDistinctCapOnce = do
  countsRef <- newIORef HashMap.empty
  let wrapStateIf =
        observingStateIf
          ( \cap ->
              atomicModifyIORef'
                countsRef
                (\m -> (HashMap.insertWith (+) (show cap) (1 :: Int) m, ()))
          )
  (_hmf, startTest) <- initCompEngineTestWith wrapStateIf compDefs
  startTest CompRunUnlimitedIterations shouldStartNextRun () (\_ -> pure ())
  counts <- readIORef countsRef
  -- filter out "b1-main(())" itself -- capEvaluationStarted also fires once
  -- for the outer comp before its body (and thus the batch) even runs; the
  -- invariant under test is about the 10 distinct "b1-leaf" caps the batch
  -- names, not the single comp that issues it.
  let leafCounts = HashMap.filterWithKey (\k _ -> "b1-leaf" `L.isPrefixOf` k) counts
  assertEqual 10 (HashMap.size leafCounts)
  assertEqual (replicate 10 (1 :: Int)) (HashMap.elems leafCounts)
 where
  compDefs = do
    leaf <- wireComp b1LeafCompDef
    wireComp (b1MainCompDef leaf)
  shouldStartNextRun :: HashMapFlow -> Int -> Bool -> Int -> () -> IO (NextRun, ())
  shouldStartNextRun _ 1 _ _ () = pure (startNextRun, ())
  shouldStartNextRun _ _ _ _ () = pure (noNextRun, ())

----------------------------------------------------------------------------
-- B.2: golden ordering trace across all three leaf request kinds (source
-- read, cap evaluation, sink write) plus a nested eval, mixed inside one
-- applicative batch. Frozen by construction -- see the module haddock.
----------------------------------------------------------------------------

data TraceReq a where
  TraceReadReq :: String -> TraceReq String

-- | A minimal 'CompSrc' that appends @"src <name> <key>"@ to a shared trace
-- on every read, so a test can observe exactly when (relative to cap
-- evaluations and sink writes) each source instance was actually asked for
-- data.
data TraceSrc = TraceSrc
  { trs_name :: T.Text
  , trs_traceRef :: IORef [String]
  }
  deriving (Typeable)

instance CompSrc TraceSrc where
  type CompSrcReq TraceSrc = TraceReq
  type CompSrcKey TraceSrc = String
  type CompSrcVer TraceSrc = Int
  compSrcInstanceId = CompSrcInstanceId . trs_name
  compSrcExecute src (TraceReadReq key) =
    do
      let entry = "src " ++ T.unpack (trs_name src) ++ " " ++ key
      atomicModifyIORef' (trs_traceRef src) (\xs -> (xs ++ [entry], ()))
      pure (HashSet.singleton (Dep key (0 :: Int)), Ok (T.unpack (trs_name src) ++ ":" ++ key))
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry

  -- Both TraceSrc instances below (b2SrcAId/b2SrcBId) are safe to run
  -- concurrently -- 'compSrcExecute' only appends to a shared IORef via
  -- 'atomicModifyIORef''. Declaring this doesn't change
  -- 'test_goldenOrderingTraceAcrossSrcEvalSink' (that test's registry never
  -- calls 'setCompFlowConcurrency', so it stays at the default width of 1,
  -- where a 'FlowConcurrent' declaration is inert -- see
  -- 'compSrcConcurrency'\'s haddock); it's what lets
  -- 'test_goldenOrderingTraceEvalSinkSubsequenceUnchangedAtWidth8' below
  -- actually exercise the job path against the very same trace fixture.
  compSrcConcurrency _ = FlowConcurrent

data TraceWriteReq a where
  TraceWriteReq :: String -> TraceWriteReq ()

-- | The sink-side counterpart of 'TraceSrc': appends @"sink <name>
-- <value>"@ on every write.
data TraceSink = TraceSink
  { trk_name :: T.Text
  , trk_traceRef :: IORef [String]
  }
  deriving (Typeable)

instance CompSink TraceSink where
  type CompSinkReq TraceSink = TraceWriteReq
  type CompSinkOut TraceSink = String
  compSinkInstanceId = CompSinkInstanceId . trk_name
  compSinkExecute sink (TraceWriteReq val) =
    do
      let entry = "sink " ++ T.unpack (trk_name sink) ++ " " ++ val
      atomicModifyIORef' (trk_traceRef sink) (\xs -> (xs ++ [entry], ()))
      pure (HashSet.singleton val, Ok ())
  compSinkDeleteOutputs _ _ = pure ()
  compSinkListExistingOutputs _ = None

-- Top-level 'unsafeMkTypedCompSrcId'\/'unsafeMkTypedCompSinkId' CAFs, for
-- the same reason as 'TestHelper.hashMapSrcId': the comp defs below are
-- built before any 'TraceSrc'\/'TraceSink' instance exists.
b2SrcAId :: TypedCompSrcId TraceSrc
b2SrcAId = unsafeMkTypedCompSrcId (Proxy @TraceSrc) "b2-srcA"

b2SrcBId :: TypedCompSrcId TraceSrc
b2SrcBId = unsafeMkTypedCompSrcId (Proxy @TraceSrc) "b2-srcB"

b2SinkId :: TypedCompSinkId TraceSink
b2SinkId = unsafeMkTypedCompSinkId (Proxy @TraceSink) "b2-sink"

b2InnerCompDef :: CompDef Int Int
b2InnerCompDef = defineComp "b2-inner" fullCaching $ \p -> pure (p + 1)

-- | The nested eval: 'b2-leaf's own body evaluates 'b2-inner', so its cap
-- evaluation happens *inside* the outer batch's processing of 'b2-leaf',
-- not alongside it.
b2LeafCompDef :: Comp Int Int -> CompDef Int Int
b2LeafCompDef inner =
  defineComp "b2-leaf" fullCaching $ \p ->
    do
      Just v <- evalComp inner p
      pure v

-- | The single applicative batch mixing all three leaf request kinds: a
-- source read (srcA), a cap evaluation (b2-leaf, itself nesting a second
-- cap evaluation), and a second source read (srcB) from a *different*
-- instance -- then, outside the batch, a sink write.
b2MainCompDef
  :: TypedCompSrcId TraceSrc
  -> TypedCompSrcId TraceSrc
  -> Comp Int Int
  -> TypedCompSinkId TraceSink
  -> CompDef () ()
b2MainCompDef srcAId srcBId leaf sinkId =
  defineComp "b2-main" inMemoryShowCaching $ \() ->
    do
      _ <-
        (,,)
          <$> compSrcReq srcAId (TraceReadReq "keyA")
          <*> evalCompOrFail leaf 1
          <*> compSrcReq srcBId (TraceReadReq "keyB")
      compSinkReq sinkId (TraceWriteReq "result")

test_goldenOrderingTraceAcrossSrcEvalSink :: IO ()
test_goldenOrderingTraceAcrossSrcEvalSink =
  do
    traceRef <- newIORef []
    let srcA = TraceSrc{trs_name = "b2-srcA", trs_traceRef = traceRef}
        srcB = TraceSrc{trs_name = "b2-srcB", trs_traceRef = traceRef}
        sink = TraceSink{trk_name = "b2-sink", trk_traceRef = traceRef}
    reg <- newCompFlowRegistry
    registerCompSrc reg srcA
    registerCompSrc reg srcB
    registerCompSink reg sink
    (rawStateIf, closeSif) <- initStateIf True
    let stateIf =
          observingStateIf
            (\cap -> atomicModifyIORef' traceRef (\xs -> (xs ++ ["eval " ++ show cap], ())))
            rawStateIf
    (_compMap, mainComp) <- failInM $ runCompWireM compDefs
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
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
    runCompEngine ifs caps rifs () `finally` closeSif
    trace <- readIORef traceRef
    assertEqual
      [ "eval b2-main(())"
      , "src b2-srcA keyA"
      , "eval b2-leaf(1)"
      , "eval b2-inner(1)"
      , "src b2-srcB keyB"
      , "sink b2-sink result"
      ]
      trace
 where
  compDefs =
    do
      inner <- wireComp b2InnerCompDef
      leaf <- wireComp (b2LeafCompDef inner)
      wireComp (b2MainCompDef b2SrcAId b2SrcBId leaf b2SinkId)

{- | B.2's companion at width 8: the golden trace literal above is frozen
 (see the module haddock) and must not be edited, but concurrency is
 allowed to change *when* a job-eligible source leaf's read lands relative
 to the batch's other leaves -- see 'prepSrcLeaf's haddock in "Impl". Both
 'TraceSrc' instances now declare 'FlowConcurrent' (see above), so at width
 8 srcA's and srcB's reads both run during the batch's dispatch-then-drain
 phase, before any of the batch's engine-thread work (including "eval
 b2-leaf(1)") even starts -- their relative order to *each other* is
 whatever the OS scheduler picks, and their position relative to the eval\/
 sink entries can float. What must NOT move is the eval\/sink entries'
 order relative to *each other*: filtering the src entries back out of the
 trace must reproduce the same subsequence as the golden trace above,
 proving concurrency reordered only what it's allowed to reorder.
-}
test_goldenOrderingTraceEvalSinkSubsequenceUnchangedAtWidth8 :: IO ()
test_goldenOrderingTraceEvalSinkSubsequenceUnchangedAtWidth8 =
  do
    traceRef <- newIORef []
    let srcA = TraceSrc{trs_name = "b2-srcA", trs_traceRef = traceRef}
        srcB = TraceSrc{trs_name = "b2-srcB", trs_traceRef = traceRef}
        sink = TraceSink{trk_name = "b2-sink", trk_traceRef = traceRef}
    reg <- newCompFlowRegistry
    setCompFlowConcurrency reg (mkCompFlowConcurrency 8)
    registerCompSrc reg srcA
    registerCompSrc reg srcB
    registerCompSink reg sink
    (rawStateIf, closeSif) <- initStateIf True
    let stateIf =
          observingStateIf
            (\cap -> atomicModifyIORef' traceRef (\xs -> (xs ++ ["eval " ++ show cap], ())))
            rawStateIf
    (_compMap, mainComp) <- failInM $ runCompWireM compDefs
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
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
    runCompEngine ifs caps rifs () `finally` closeSif
    trace <- readIORef traceRef
    let isSrcEntry entry = "src " `L.isPrefixOf` entry
        (srcEntries, evalSinkEntries) = L.partition isSrcEntry trace
    assertEqual
      [ "eval b2-main(())"
      , "eval b2-leaf(1)"
      , "eval b2-inner(1)"
      , "sink b2-sink result"
      ]
      evalSinkEntries
    assertEqual
      (HashSet.fromList ["src b2-srcA keyA", "src b2-srcB keyB"])
      (HashSet.fromList srcEntries)
 where
  compDefs =
    do
      inner <- wireComp b2InnerCompDef
      leaf <- wireComp (b2LeafCompDef inner)
      wireComp (b2MainCompDef b2SrcAId b2SrcBId leaf b2SinkId)

----------------------------------------------------------------------------
-- B.3: compMAp keeps the LEFT error when both applicative branches fail,
-- but still runs (and records dependencies for) both -- see compMAp's
-- haddock in Types.hs.
----------------------------------------------------------------------------

-- | Directly exercises 'Control.Computations.CompEngine.Types.compMAp's
-- case analysis (no engine required): both branches are already-failed
-- 'CompM' actions, so combining them via @\<*\>@ hits the
-- @(CompFinished (CompResultFail e), _)@ case immediately, and the kept
-- error must be the left one.
test_compMApKeepsLeftErrorMessageOnBothFailure :: IO ()
test_compMApKeepsLeftErrorMessageOnBothFailure =
  do
    depsRef <- newIORef []
    let env = CompMEnv{cme_compMap = Map.empty, cme_deps = depsRef}
        combined = (,) <$> (fail "A failed" :: CompM Int) <*> (fail "B failed" :: CompM Int)
    CompFinished result <- runCompM combined env
    assertEqual (CompResultFail "A failed") result

-- | A source with two requests: 'ReadA' always fails at a constant version
-- (so it alone never causes staleness), 'ReadB' succeeds and reflects
-- 'ds_bVal', whose version 'bumpB' can change from outside a comp body --
-- exactly what's needed to drive B.3's second assertion.
data DualReq a where
  ReadA :: DualReq Int
  ReadB :: DualReq Int

data DualSrc = DualSrc
  { ds_bVal :: TVar Int
  , ds_changes :: TVar (HashSet.HashSet (Dep String Int))
  }
  deriving (Typeable)

initDualSrc :: IO DualSrc
initDualSrc = DualSrc <$> newTVarIO 0 <*> newTVarIO HashSet.empty

-- | Change 'ReadB's value, recording the new version as a change so the
-- driver loop's 'allCompSrcChanges' wakes up and invalidates every cap that
-- observed the old version.
bumpB :: DualSrc -> Int -> IO ()
bumpB src v =
  atomically $
    do
      writeTVar (ds_bVal src) v
      modifyTVar' (ds_changes src) (HashSet.insert (Dep "B" v))

instance CompSrc DualSrc where
  type CompSrcReq DualSrc = DualReq
  type CompSrcKey DualSrc = String
  type CompSrcVer DualSrc = Int
  compSrcInstanceId _ = CompSrcInstanceId "DualSrc"
  compSrcExecute _ ReadA =
    pure (HashSet.singleton (Dep "A" 0), Fail "A failed")
  compSrcExecute src ReadB =
    do
      v <- readTVarIO (ds_bVal src)
      pure (HashSet.singleton (Dep "B" v), Ok v)
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges src =
    do
      changes <- readTVar (ds_changes src)
      writeTVar (ds_changes src) HashSet.empty
      if HashSet.null changes then retry else pure changes

b3SrcId :: TypedCompSrcId DualSrc
b3SrcId = unsafeMkTypedCompSrcId (Proxy @DualSrc) "DualSrc"

-- | Fails unconditionally (via 'ReadA'), but its dependency on 'ReadB' --
-- the branch whose value is discarded by @compMAp@'s left bias -- must
-- still be recorded, so a later change to B alone still marks this comp
-- stale.
b3MainCompDef :: CompDef () ()
b3MainCompDef =
  defineComp "b3-main" fullCaching $ \() ->
    void ((,) <$> compSrcReq b3SrcId ReadA <*> compSrcReq b3SrcId ReadB)

test_compMApDoesNotLoseDepsFromTheDiscardedBranchOnFailure :: IO ()
test_compMApDoesNotLoseDepsFromTheDiscardedBranchOnFailure =
  do
    src <- initDualSrc
    reg <- newCompFlowRegistry
    registerCompSrc reg src
    (rawStateIf, closeSif) <- initStateIf True
    evalCountsRef <- newIORef (0 :: Int)
    let stateIf = observingStateIf (\_cap -> atomicModifyIORef' evalCountsRef (\n -> (n + 1, ()))) rawStateIf
        -- run 1: initial settle (already covered by startCompEngine below,
        -- so this finds nothing new); run 2: bump B alone right before the
        -- driver checks for changes, so this run's processing picks it up;
        -- run 3: stop.
        shouldStartNextRun :: Int -> Bool -> Int -> () -> IO (NextRun, ())
        shouldStartNextRun 1 _ _ () = pure (startNextRun, ())
        shouldStartNextRun 2 _ _ () = bumpB src 1 >> pure (startNextRun, ())
        shouldStartNextRun _ _ _ () = pure (noNextRun, ())
    (_compMap, mainComp) <- failInM $ runCompWireM (wireComp b3MainCompDef)
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
        caps = [wrapCompAp (mkCompAp mainComp ())]
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = shouldStartNextRun
            , rcif_emptyChangesMode = DontBlock
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = seconds 10
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = \_ -> pure ()
            }
    runCompEngine ifs caps rifs () `finally` closeSif
    -- once for the initial evaluation (startCompEngine), once more after
    -- bumping B alone -- proving B's dependency (the branch compMAp's left
    -- bias discarded the *value* of) was not lost.
    finalCount <- readIORef evalCountsRef
    assertEqual 2 finalCount
