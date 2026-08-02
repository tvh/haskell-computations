{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Computations.CompEngine.Impl (
  CompEngine,
  GenDel,
  garbage,
  startCompEngine,
  stopCompEngine,
  notifyCompEngine,
  stepCompEngine,
  initCompEngine,
  evalWithCompEngine,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.ConcUtils (dispatchJobs, trySync)
import Control.Computations.Utils.Logging
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Exception (SomeException, throwIO)
import Control.Monad
import Control.Monad.Reader
-- Strict, not the mtl default (Control.Monad.State re-exports the Lazy
-- variant) -- see CompEngineM's Monad instance below for why this matters
-- on the suspend/resume hot path.
import Control.Monad.State.Strict
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Proxy (..), cast)
import Data.Word (Word64)

data CompEngine = CompEngine
  { ce_compEngineIfs :: CompEngineIfs
  , ce_capSeqCounter :: IORef Word64
  -- ^ see 'CapSeq'\'s haddock -- one monotone counter, shared by every
  -- 'tellGarbage' call for the lifetime of this engine, so stamps compare
  -- correctly across every accumulation window, not just within one.
  }

{- | A monotone "who touched this cap last, and how" stamp, one per
 'AnyCompAp', carried alongside a 'GenDel' accumulation so a cap's
 revive-vs-free history within one accumulation window can be resolved by a
 commutative 'Semigroup' instead of by mutating a shared set in call order.

 'CompEngineM' is @StateT GenDel@, and a later parallel-evaluation project
 needs to merge per-worker 'GenDel' accumulators on join -- 'GenDel'\'s
 other fields were already commutative unions, but 'tellGarbage' (see its
 haddock) was not: it deleted a key from the *whole accumulated* set,
 which only means what it is supposed to mean ("the most recent thing that
 happened to this cap was a revival") if deletions apply in call order. A
 plain merged union of two workers' accumulators has no call order to
 apply them in.

 The obvious fix -- track revived caps additively in a separate set,
 subtract it from the freed set once at the end -- is wrong, and wrong
 already at width 1 (single-threaded, no merging involved at all).
 Counterexample, entirely within one accumulation window: cap X is
 re-evaluated successfully, so its revival is recorded; later in the same
 window X's last dependent stops depending on it, so a cascade reports X
 as freed. Today (and under this stamp scheme) X ends up freed, because
 that is the *last* thing that happened to it, and its output is correctly
 deleted. Additively, "X was revived at some point" and "X was freed at
 some point" are both true and neither is more recent than the other, so
 subtracting the revived set from the freed set unconditionally erases X
 from the freed set -- and a dead output is never deleted. See
 @test_reviveThenFreeInSameRoundDeletesOutput@ in TestRevive.hs, which
 exists specifically to catch a future refactor sliding back to this
 additive shape.

 Order therefore has to be encoded in the values being merged, not in the
 order the merge happens to run in -- a monotone counter stamped onto
 every event, combined with 'max', does that: whichever stamp is larger
 (freed's or revived's) is definitionally the more recent event, regardless
 of what order the two 'GenDel' fragments carrying them get '<>'-ed
 together. At width 1 the counter's issue order *is* wall-clock/call order
 (see 'ce_capSeqCounter'), so @cs_revived > cs_freed@ after merging is
 exactly "the last event for this cap was a revive" -- bit-identical to
 today's mutate-in-call-order behaviour, including the counterexample
 above (checked directly, not just argued: see the test cited above).
-}
data CapSeq = CapSeq
  { cs_freed :: !Word64
  , cs_revived :: !Word64
  }
  deriving (Show, Eq)

instance Semigroup CapSeq where
  CapSeq f1 r1 <> CapSeq f2 r2 = CapSeq (max f1 f2) (max r1 r2)

instance Monoid CapSeq where
  mempty = CapSeq 0 0

{- | Track generated outputs and index them by the computation that created them.
 See test_oneComputationRecomputedButOtherUnreferencesIt in TestOutputs
 for why this is necessary.
 Also track unreachable/garbage-collectable caps and outputs.
 Be sure to only delete outputs that have not been generated as well and
 also make sure that there are no stale caps when performing the deletions
 because one of those stale caps might generate the to-be-deleted output.
 See test in TestOutputs for a case where this happens.
-}
data GenDel = GenDel
  { _gd_generated :: Map AnyCompAp AnyCompSinkOutsMap
  , _gd_garbage :: Garbage
  , _gd_capSeq :: HashMap AnyCompAp CapSeq
  -- ^ see 'CapSeq'\'s haddock. Merged via its own commutative 'Semigroup'
  -- ('max' per field), same as every other 'GenDel' field -- unlike
  -- '_gd_garbage'\'s 'garbage_caps', nothing here ever needs a targeted
  -- deletion.
  }
  deriving (Show, Eq)

instance Semigroup GenDel where
  (<>) (GenDel a1 b1 c1) (GenDel a2 b2 c2) =
    GenDel (Map.unionWith unionAnyCompSinkOutsMap a1 a2) (b1 <> b2) (HashMap.unionWith (<>) c1 c2)

instance Monoid GenDel where
  mempty = GenDel mempty mempty mempty

generated :: AnyCompAp -> AnyCompSinkOutsMap -> GenDel
generated k mo = GenDel (Map.singleton k mo) mempty mempty

deleted :: Garbage -> GenDel
deleted g = GenDel mempty g mempty

capSeqStamped :: HashMap AnyCompAp CapSeq -> GenDel
capSeqStamped = GenDel mempty mempty

{- | Resolve 'garbage_caps' against '_gd_capSeq': a cap accumulated into
 @garbage_caps@ at some point during this window (an ordinary, commutative
 union -- see 'tellGarbage') is only *actually* garbage if the last thing
 that happened to it, per its merged 'CapSeq', was a free rather than a
 revive. See 'CapSeq'\'s haddock for why this comparison, not the presence
 of the key in @garbage_caps@ itself, is what decides membership now.
-}
garbage :: GenDel -> Garbage
garbage (GenDel gen (Garbage garbage_caps garbage_deps garbage_outputs) capSeq) =
  -- Keep only caps whose most recent event (by merged CapSeq) was a free,
  -- not a revive -- see this function's haddock.
  let stillGarbage cap = case HashMap.lookup cap capSeq of
        Just (CapSeq freedAt revivedAt) -> revivedAt <= freedAt
        Nothing -> True
      garbage_caps' = HashSet.filter stillGarbage garbage_caps
      -- First delete any outputs from the generated outputs
      -- that belong to a garbage cap
      genWithCapsDeleted =
        foldr Map.delete gen (HashSet.toList garbage_caps')
      -- Then remove the resulting set of outputs from any garbage outputs
      result =
        let garbageOutputsRegeneratedFiltered =
              fmap
                ( `diffAnyOutsMap`
                    (unionsAnyCompSinkOutsMap $ Map.elems genWithCapsDeleted)
                )
                garbage_outputs
            garbageOutputsTrimmed =
              HashMap.filter (not . nullAnyOutsMap) garbageOutputsRegeneratedFiltered
         in Garbage garbage_caps' garbage_deps garbageOutputsTrimmed
   in if isGarbageEmpty result
        then result
        else pureDebug ("Returning garbage: " ++ show result) result

newtype CompEngineM a = CompEngineM {unCompEngineM :: StateT GenDel (ReaderT CompEngine IO) a}

{- | Hand-written rather than @deriving (Functor, Applicative, Monad,
 MonadIO)@ (via GeneralizedNewtypeDeriving): the suspend/resume hot path
 (`loop`/`doSuspended`) performs a real monadic bind on every round trip
 through the engine, so any per-bind overhead here is paid once per
 suspension point across an entire evaluation. A derived instance's
 dictionary-passing does not reliably specialize away under -O2, which
 leaves a live, allocating `$fMonadCompEngineM_$s$fMonadStateT_$c>>=` cost
 center sitting on that path. Each method here is a one-line
 unwrap-delegate-rewrap around the identical underlying
 `StateT`/`ReaderT`/`IO` operation (so this is representationally a no-op,
 same as what deriving would generate), but the explicit INLINE pragma
 gives GHC's simplifier a direct mandate to substitute the body at every
 call site and keep specializing through `mtl`'s own (also INLINE-marked)
 StateT/ReaderT instances down to concrete IO, rather than leaving a
 standalone dictionary method for the profiler (and the runtime) to keep
 finding.
-}
instance Functor CompEngineM where
  fmap f (CompEngineM m) = CompEngineM (fmap f m)
  {-# INLINE fmap #-}

instance Applicative CompEngineM where
  pure = CompEngineM . pure
  {-# INLINE pure #-}
  CompEngineM mf <*> CompEngineM mx = CompEngineM (mf <*> mx)
  {-# INLINE (<*>) #-}

instance Monad CompEngineM where
  CompEngineM m >>= f = CompEngineM (m >>= unCompEngineM . f)
  {-# INLINE (>>=) #-}

instance MonadIO CompEngineM where
  liftIO = CompEngineM . liftIO
  {-# INLINE liftIO #-}

{- | The result of preparing one leaf of a 'CompReqCombined' batch (see
 'Control.Computations.CompEngine.Types.traverseCompReq'): the engine-thread
 computation that actually produces its value.

 Spelled out, this is @Compose IO (Compose CompEngineM CompM)@ written by
 hand rather than pulled in from "Data.Functor.Compose" -- a composition of
 lawful 'Applicative's is itself a lawful 'Applicative' (that's the whole
 point of 'Compose'), so 'Prep'\'s own laws follow from its three layers'
 each being exactly what they claim:

  * the outer 'IO' layer is where 'prepSrcLeaf' resolves a source leaf's
    'Control.Computations.CompEngine.CompSrc.CompSrc' instance (one registry
    lookup) and files this leaf's request into that instance's 'SrcGroup' --
    every leaf sharing an instance across the whole batch lands in the same
    group, discovered incrementally as this IO layer runs leaf by leaf, left
    to right (see 'getOrCreateGroup'). Every other leaf kind has nothing to
    file, so for them this layer just 'pure's its result;
  * 'CompEngineM' is the engine-thread computation that does this leaf's
    actual work -- cache lookups, 'compSinkExecute' calls, recursive cap
    evaluation, or (for a source leaf) triggering its group's bundled
    'Control.Computations.CompEngine.CompSrc.compSrcExecuteBatch' call, if no
    other leaf in the group has already, and then reading this leaf's own
    result back out of its slot -- see 'runGroupOnce'. Its 'Applicative'
    sequences effects strictly left to right (see its instance above), which
    is what keeps leaf order identical to the recursive walk 'doSuspended'
    used before this type existed -- the golden trace in
    "Control.Computations.CompEngine.Tests.TestCompReqCombined" is pinned on
    exactly that, and it is also what gives the leftmost failing leaf's
    exception priority over any leaf to its right (see 'runGroupOnce');
  * 'CompM' is the leaf's own deferred result. Combining two leaves'
    'CompM's uses 'CompM'\'s own '<*>' ('compMAp') rather than a hand-rolled
    combinator: reusing it is what preserves 'compMAp'\'s deliberate
    left-error bias and its "both sides always run" dependency-tracking
    guarantee (see its haddock) for a batch assembled this way, instead of
    quietly re-deciding those rules a second time here.
-}
newtype Prep a = Prep {unPrep :: IO (CompEngineM (CompM a))}

instance Functor Prep where
  fmap f (Prep io) = Prep (fmap (fmap (fmap f)) io)
  {-# INLINE fmap #-}

instance Applicative Prep where
  pure x = Prep (pure (pure (pure x)))
  {-# INLINE pure #-}
  Prep iof <*> Prep iox = Prep $ do
    mf <- iof
    mx <- iox
    -- The outer `<*>` is CompEngineM's (left-to-right effect sequencing);
    -- the inner one, fmapped in, is CompM's own `<*>` -- i.e. `compMAp`.
    pure ((<*>) <$> mf <*> mx)
  {-# INLINE (<*>) #-}

{- | Mutable, per-'CompSrc'-instance state accumulated while preparing one
 'CompReqCombined' batch: every 'SrcFetch' any leaf of the batch has
 contributed so far (built as a difference list, the same O(1)-append trick
 "Control.Computations.Utils.ConcUtils".'Control.Computations.Utils.ConcUtils.dispatchJobs'\'s
 own caller used to use for 'SrcJobs', for the same reason -- a batch of
 thousands of same-instance leaves must not re-copy this list on every
 append), plus a run-once guard so the group's bundled
 'Control.Computations.CompEngine.CompSrc.compSrcExecuteBatch' call fires
 exactly once no matter which of its member leaves reaches it first, or
 whether the engine proactively ran it as a dispatched job before any leaf
 was even reached (see 'runGroupOnce').
-}
data SrcGroup s = SrcGroup
  { sg_src :: s
  , sg_conc :: FlowConcurrency
  , sg_fetches :: IORef ([SrcFetch s] -> [SrcFetch s])
  , sg_triggered :: IORef Bool
  }

-- | A 'SrcGroup' with its instance type hidden -- what
-- 'Control.Computations.CompEngine.Impl.doSuspended' actually stores one of
-- per distinct source instance seen while preparing a batch, since different
-- leaves resolve their (possibly different) source types independently (see
-- 'getOrCreateGroup').
data SomeSrcGroup = forall s. CompSrc s => SomeSrcGroup (SrcGroup s)

{- | Find this leaf's 'SrcGroup' in @groupsRef@, creating an empty one keyed
 by @ix@ if this is the first leaf of the batch to resolve to this instance.
 @groupsRef@ is a plain association list, not a 'HashMap.HashMap': a batch
 sees only 1 to 5 distinct source instances in practice, however many
 thousand leaves, so a handful of 'Eq' comparisons against a dense
 'CompSrcInstIx' (assigned once per instance at registration -- see its
 haddock) beats hashing a 'CompSrcId' (two 'Data.Text.Text' values) on every
 single leaf, which is what keying on 'CompSrcId' itself would cost here.

 The index alone doesn't prove @g@'s hidden type matches @s@: two leaves
 resolving the same instance independently (through their own, syntactically
 distinct existential @s@ -- see 'CompLeafSrc') still need a runtime check
 before either can be treated as the other's @s@. The registry maps one
 index to exactly one concrete type, so the 'cast' below can only fail if
 that invariant has been broken elsewhere, not from anything a caller here
 can cause.
-}
getOrCreateGroup
  :: forall s
   . CompSrc s
  => IORef [(CompSrcInstIx, SomeSrcGroup)]
  -> CompSrcInstIx
  -> s
  -> FlowConcurrency
  -> IO (SrcGroup s)
getOrCreateGroup groupsRef ix src conc = do
  groups <- readIORef groupsRef
  case lookup ix groups of
    Just (SomeSrcGroup g) ->
      case cast g of
        Just g' -> pure g'
        Nothing ->
          error
            ( "getOrCreateGroup: "
                ++ show (compSrcId src)
                ++ " resolved to two different CompSrc types within one batch "
                ++ "-- should be impossible, the registry maps each id to exactly one type"
            )
    Nothing -> do
      fetchesRef <- newIORef id
      triggeredRef <- newIORef False
      let g = SrcGroup src conc fetchesRef triggeredRef
      writeIORef groupsRef ((ix, SomeSrcGroup g) : groups)
      pure g

{- | Run @g@\'s bundled 'compSrcExecuteBatch' call exactly once, however many
 times (and from however many threads) this is called for the same group:
 the 'sg_triggered' guard makes every call after the first a no-op. This is
 what lets the same function serve both dispatch paths a group can take
 (see the 'CompReqCombined' case of 'doSuspended'):

  * a 'FlowConcurrent' group dispatched as a job calls this from a
    'Control.Computations.Utils.ConcUtils.dispatchJobs' worker, before
    'enginePhase' runs at all -- by the time any member leaf's own
    'CompEngineM' action (built by 'prepSrcLeaf', below) reaches this same
    call, it is already a no-op and just reads its own slot;
  * a group that was never dispatched (a 'FlowSerial' instance, or any
    instance at width 1 -- see 'CompFlowConcurrency') is never proactively
    triggered, so this call happens for the first time when the *first* of
    its member leaves is reached inside 'enginePhase'\'s left-to-right run,
    exactly the position a single, unbundled 'compSrcExecute' call would
    have run at before this group existed. This is deliberate: bundling
    genuinely needs no worker thread, so it is applied even at width 1 (see
    "Control.Computations.CompEngine.CompSrc".'Control.Computations.CompEngine.CompSrc.compSrcExecuteBatch'\'s
    haddock) -- but doing so must not change *when*, relative to the rest of
    the batch's leaves, this instance's data is actually asked for, which is
    exactly what the golden ordering trace in
    "Control.Computations.CompEngine.Tests.TestCompReqCombined" pins down.

 Guarded by 'Control.Computations.Utils.ConcUtils.trySync' at the group
 level, on top of whatever fault isolation @compSrcExecuteBatch@ itself
 provides per request (the default does, via the same guard -- see its
 haddock): an override that does not isolate its own requests and simply
 lets an exception escape the whole call still can't strand any member
 leaf's slot unfilled -- every fetch in the group gets the same exception
 written to it, so whichever leaf 'enginePhase' reaches first still gets to
 report it, preserving the leftmost-failing-leaf guarantee at the group
 level too.
-}
runGroupOnce :: forall s. CompSrc s => SrcGroup s -> IO ()
runGroupOnce g = do
  alreadyRan <- atomicModifyIORef' (sg_triggered g) (\ran -> (True, ran))
  unless alreadyRan $ do
    fetches <- ($ []) <$> readIORef (sg_fetches g)
    result <- trySync (compSrcExecuteBatch (sg_src g) fetches)
    case result of
      Right () -> pure ()
      Left ex -> forM_ fetches $ \(SrcFetch _ write) -> write (Left ex)

{- | Record @g@ as garbage collected while finishing @mKey@\'s evaluation
 (@mKey@ is 'Nothing' when that evaluation failed), and, if @mKey@ is a
 'Just' that has been freed earlier in this same accumulation window,
 stamp it as revived -- see 'CapSeq'\'s haddock for why a stamp replaces
 the old "delete the cap that generated the garbage from the accumulated
 garbage set" mutation (still the same underlying purpose: avoid later
 marking output as garbage that might have been regenerated by this cap;
 see @test_dontDeleteAfterRevival@ in TestRevive).

 @base@ and @base + 1@ are reserved for this one call: every cap freed by
 @g@ is stamped @base@, and @mKey@ (when it gets a revival stamp at all)
 is stamped @base + 1@ -- strictly later, encoding "the revival happens
 after this call's own freed set is merged in" (see 'CapSeq'\'s haddock
 for why that ordering within one call matters). The shared counter then
 starts the *next* call at @base + 2@, so stamps compare correctly not
 just within this call but across every call for the lifetime of the
 engine.

 @mKey@ is deliberately *not* stamped unconditionally on every successful
 evaluation -- only when it is already a member of the accumulated
 'garbage_caps' (checked after this call's own @g@ has been merged in, so
 a cap @g@ itself just froze counts too). 'garbage' only ever looks
 '_gd_capSeq' up for caps that are in 'garbage_caps' (see its haddock), so
 a stamp for any other cap is pure dead weight -- and unconditional
 stamping is not a hypothetical cost: every successful 'doCompAp' call
 passes a 'Just' here, so on a cold run of a million-cap graph where
 nothing has been freed yet, unconditional stamping would grow
 '_gd_capSeq' to a million-entry map for zero benefit, measured to add
 tens of megabytes of 'max_live_bytes' and a five-percent
 'allocated_bytes' regression on this codebase's own persist benchmark.
 Restricting to caps already in 'garbage_caps' keeps '_gd_capSeq' bounded
 by however many caps are actually freed in the window, the same
 population 'garbage_caps' itself is already bounded by.
-}
tellGarbage :: Maybe AnyCompAp -> Garbage -> CompEngineM ()
tellGarbage mKey g =
  CompEngineM $
    do
      counterRef <- asks ce_capSeqCounter
      base <- lift (lift (atomicModifyIORef' counterRef (\c -> (c + 2, c))))
      let freedSeqs =
            HashMap.fromList [(cap, CapSeq base 0) | cap <- HashSet.toList (garbage_caps g)]
      modify' (\genDel -> genDel `mappend` deleted g `mappend` capSeqStamped freedSeqs)
      accumulatedGarbageCaps <- gets (garbage_caps . _gd_garbage)
      case mKey of
        Just key | key `HashSet.member` accumulatedGarbageCaps ->
          modify' (\genDel -> genDel `mappend` capSeqStamped (HashMap.singleton key (CapSeq 0 (base + 1))))
        _ -> pure ()
      let logFun = if mempty == g then logNoLog else logDebug
      logFun ("Collected garbage " ++ show g)
{-# INLINE tellGarbage #-}

tellOutputs :: AnyCompAp -> AnyCompSinkOutsMap -> CompEngineM ()
tellOutputs key outputs =
  CompEngineM $
    do
      modify' (\genDel -> genDel `mappend` generated key outputs)
      let logFun = if nullAnyOutsMap outputs then logNoLog else logDebug
      logFun ("Outputs generated " ++ show outputs)
{-# INLINE tellOutputs #-}

runCompEngineM :: CompEngineM a -> CompEngine -> GenDel -> IO (a, GenDel)
runCompEngineM cet ce g = runReaderT (runStateT (unCompEngineM cet) g) ce
{-# INLINE runCompEngineM #-}

runCompEngineM' :: CompEngineM a -> CompEngine -> IO (a, GenDel)
runCompEngineM' cet ce = runCompEngineM cet ce mempty
{-# INLINE runCompEngineM' #-}

evalCompEngineM :: CompEngineM a -> CompEngine -> IO a
evalCompEngineM cet env =
  do
    (x, g) <- runCompEngineM' cet env
    when (garbage g /= mempty) $
      fail ("evalCompEngineM produced garbage that would be ignored: " ++ show g)
    return x
{-# INLINE evalCompEngineM #-}

initCompAp
  :: CompAp a
  -> CompM a
initCompAp cap@(CompAp _ comp p) = comp_fun comp env
 where
  env =
    CompEnv
      { ce_cachedResult = doAnyRequest $ CompReqCache cap
      , ce_param = p
      , ce_comp = comp
      }

withCompState :: (CompEngineStateIf IO -> IO a) -> CompEngineM a
withCompState mkAction =
  CompEngineM $
    do
      action <- asks (mkAction . ce_stateIf . ce_compEngineIfs)
      lift (lift action)
{-# INLINE withCompState #-}

doCompAp
  :: IsCompResult a
  => CompAp a
  -> CompEngineM (Maybe (CompApResult a))
doCompAp gap =
  do
    withCompState $ \sif -> capEvaluationStarted sif gap
    (deps, outputs, res) <- evalCompAp gap
    maybeRes <-
      case res of
        CompResultFail msg -> logNote ("Cap " ++ show gap ++ " failed: " ++ msg) >> return Nothing
        CompResultOk ok ->
          do
            logDebug ("Cap " ++ show gap ++ " succeeded.")
            return (Just ok)
    (staleCaps, gcCaps) <-
      withCompState $ \sif ->
        capEvaluationFinished sif gap deps outputs (fmap cr_returnValue maybeRes)
    tellGarbage (maybeRes >> Just (AnyCompAp gap)) gcCaps
    logStale ("Eval of " ++ show gap) staleCaps
    return maybeRes

evalCompAp
  :: forall a
   . IsCompResult a
  => CompAp a
  -> CompEngineM (DepSet, AnyCompSinkOutsMap, CompResult (CompApResult a))
evalCompAp outerCap =
  do
    logDebug ("Evaluating " ++ show outerCap)
    -- Fresh per-cap-evaluation dependency and sink-output accumulators (see
    -- CompMEnv's haddock in Types.hs). `env` is threaded explicitly through
    -- every helper below rather than captured via a Reader in CompEngineM:
    -- its lifetime is scoped to exactly this one evaluation, and explicit
    -- threading makes that scoping visible instead of relying on someone
    -- remembering to reset a Reader-carried ref between nested cap evals.
    depsRef <- liftIO (newIORef [])
    outputsRef <- liftIO (newIORef mempty)
    let env = CompMEnv{cme_compMap = r, cme_deps = depsRef, cme_outputs = outputsRef}
    finalResult <- loop env (initCompAp outerCap)
    accumulated <- liftIO (readIORef depsRef)
    outputs <- liftIO (readIORef outputsRef)
    let !deps = HashSet.fromList accumulated
        !ev = fmap (compApResult outerCap) finalResult
    logDebug
      ( show outerCap
          ++ " --> "
          ++ ( case ev of
                -- CompCacheMeta does not pre-render or store a logrepr
                -- (see its haddock in Types.hs), so render the
                -- already-in-scope typed value directly, on demand, only
                -- for this log line.
                CompResultOk (CompApResult val ccv) ->
                  concat
                    [ take 40 (show val)
                    , " ("
                    , show (ccv_largeHash ccv)
                    , ")"
                    ]
                CompResultFail msg -> msg
             )
      )
    return (deps, outputs, ev)
 where
  r =
    case outerCap of
      CompAp _ comp _ -> comp_compMap comp
  -- | Routed through 'doAnyEvalReqValue' rather than calling 'evalWithCache'
  -- directly: inlining both sides shows this is not just monad-law
  -- equivalent but literally the same code --
  -- @evalWithCache env False innerCap k (doCompAp innerCap)@ unfolds (via
  -- 'evalWithCache's own definition) to exactly
  -- @evalWithCacheValue False innerCap (doCompAp innerCap) >>= \x -> loop env
  -- (contToCompM (k x))@, which is 'doAnyEvalReqValue' unfolded the same
  -- way. A later promise table needs to wrap every place a cap gets
  -- evaluated-or-looked-up; routing the CPS entry point through the value
  -- one collapses that to a single site instead of two that must be kept in
  -- sync by hand.
  doAnyEvalReq
    :: forall a x
     . IsCompResult x
    => CompMEnv
    -> CompAp x
    -> CompCont (Maybe (CompApResult x)) a
    -> CompEngineM (CompResult a)
  doAnyEvalReq env innerCap k =
    doAnyEvalReqValue innerCap >>= \x -> loop env (contToCompM (k x))

  doAnyCacheReq
    :: forall a x
     . IsCompResult x
    => CompMEnv
    -> CompAp x
    -> CompCont (Maybe x) a
    -> CompEngineM (CompResult a)
  doAnyCacheReq env innerCap k =
    evalWithCache env True innerCap (k . fmap cr_returnValue) (return Nothing)

  -- | Re-expressed in terms of 'evalWithCacheValue': by the monad laws,
  -- @withCapLookup cap f pure (evalWithCapCached cap f pure staleOk)
  -- capLookup >>= cont@ is the same computation as
  -- @withCapLookup cap f cont (evalWithCapCached cap f cont staleOk)
  -- capLookup@ for any @cont@ -- every branch of both either calls @cont@
  -- (resp. @pure >=> cont@, which is just @cont@) directly, or calls
  -- @f >>= cont@ (resp. @(f >>= pure) >>= cont@, which associativity and
  -- the left-identity law collapse to the same @f >>= cont@). So this is
  -- exactly today's code, just with the "evaluate-or-look-up" step and the
  -- "resume the caller's continuation" step pulled apart.
  evalWithCache
    :: forall a x
     . IsCompResult x
    => CompMEnv
    -> Bool
    -> CompAp x
    -> CompCont (Maybe (CompApResult x)) a
    -> CompEngineM (Maybe (CompApResult x))
    -> CompEngineM (CompResult a)
  evalWithCache env staleOk cap k f =
    evalWithCacheValue staleOk cap f >>= \x -> loop env (contToCompM (k x))

  -- | The value form of 'evalWithCache': runs the same
  -- evaluate-or-look-up-cache logic (via 'withCapLookup'\/'evalWithCapCached',
  -- both already polymorphic in their result type, here just instantiated
  -- at their continuation @= pure@) but stops at the raw result instead of
  -- resuming a suspended caller's continuation through 'loop'. This is what
  -- lets a 'CompReqCombined' leaf be prepared as a value (see 'Prep')
  -- rather than as a CPS step.
  evalWithCacheValue
    :: forall x
     . IsCompResult x
    => Bool
    -> CompAp x
    -> CompEngineM (Maybe (CompApResult x))
    -> CompEngineM (Maybe (CompApResult x))
  evalWithCacheValue staleOk cap f =
    withCompState (flip lookupCapResult cap)
      >>= withCapLookup cap f pure (evalWithCapCached cap f pure staleOk)

  evalWithCapCached
    :: forall a b
     . IsCompResult a
    => CompAp a
    -> CompEngineM (Maybe (CompApResult a))
    -> (Maybe (CompApResult a) -> CompEngineM b)
    -> Bool
    -> CapResult (CapCached a)
    -> CompEngineM b
  evalWithCapCached cap f cont staleOk capCached =
    case capCached of
      CapSuccess (CapValueCached a)
        | staleOk -> cont (Just a)
        | otherwise ->
            withCompState (flip dequeueGivenCap cap) >>= \case
              True ->
                do
                  logInfo ("Recalculating stale cap now " ++ capName)
                  f >>= cont
              False ->
                do
                  logDebug ("Found valid cached result for " ++ capName ++ ".")
                  cont (Just a)
      CapSuccess (CapMetaCached _meta) ->
        do
          logDebug $ capName ++ " is not cached. Recalculating..."
          f >>= cont
      CapFailure
        | staleOk ->
            do
              logDebug ("Found failed result for " ++ capName ++ ".")
              cont Nothing
        | otherwise ->
            withCompState (flip dequeueGivenCap cap) >>= \case
              True -> do
                logInfo ("Recalculating previously failing, stale cap now " ++ capName)
                f >>= cont
              False ->
                do
                  logDebug ("Found valid cached failure for " ++ capName ++ ".")
                  cont Nothing
   where
    capName = show cap

  withCapLookup
    :: forall a b c
     . IsCompResult a
    => CompAp a
    -> CompEngineM (Maybe (CompApResult a))
    -> (Maybe (CompApResult a) -> CompEngineM c)
    -> (b -> CompEngineM c)
    -> CapLookup b
    -> CompEngineM c
  withCapLookup cap f cont withFound capLookup =
    case capLookup of
      CapFound found -> withFound found
      CapNotFound ->
        --  comp was never evaluated or was removed from cache
        -- NOTE: `f` runs here regardless of what `dequeueGivenCap` reports --
        -- `isStale` only picks which string the log line below uses. This is
        -- therefore *not* a mutual-exclusion point: nothing here stops two
        -- callers from both reaching `f` for the same cap. What actually
        -- prevents duplicate evaluation is that cap evaluation stays on one
        -- (the engine) thread; `f`'s result gets recorded in the cache
        -- before any other cap evaluation on that thread can observe it.
        withCompState (flip dequeueGivenCap cap) >>= \isStale ->
          do
            logDebug
              ( (if isStale then "Stale " else "Needed ")
                  ++ show (capId cap)
                  ++ " not cached.  Evaluating!"
              )
            f >>= cont

  doCompSinkReq
    :: forall x a s
     . (CompSink s)
    => CompMEnv
    -> TypedCompSinkId s
    -> CompSinkReq s a
    -> CompCont (Fail a) x
    -> CompEngineM (CompResult x)
  doCompSinkReq env sinkId req cont = do
    let sinkFun sink = do
          (outputs, res) <- liftIO $ compSinkExecute sink req
          return (wrapCompSinkOuts sink outputs, pure res)
    reg <- CompEngineM (asks (ce_compFlowRegistry . ce_compEngineIfs))
    (outputs, action) <-
      withTypedCompSinkId reg sinkId sinkFun >>= \case
        Ok y -> pure y
        Fail reason ->
          let msg = "Refusing to run request for data sink " ++ show sinkId ++ ": " ++ reason
           in pure (emptyAnyCompOutSinksMap, return (Fail msg))
    tellOutputs (AnyCompAp outerCap) outputs
    liftIO (modifyIORef' (cme_outputs env) (<> outputs))
    loop env (action >>= contToCompM . cont)

  doCompSrcReq
    :: forall x a s
     . CompSrc s
    => CompMEnv
    -> TypedCompSrcId s
    -> CompSrcReq s a
    -> CompCont (Fail a) x
    -> CompEngineM (CompResult x)
  doCompSrcReq env srcId req cont = do
    -- NOTE: computed once, outside srcFun's per-dependency mapM_ below,
    -- rather than via `wrapCompSrcDep src` inside it -- srcId is already
    -- this instance's id (it's exactly what the withTypedCompSrcId lookup
    -- just below keyed on), so re-deriving it from `src` per dependency via
    -- compSrcId would just re-render the same TypeRep to Text every time
    -- (see wrapCompSrcDep's haddock).
    let cid = unTypedCompSrcId srcId
    let srcFun src = do
          (inputs, res) <- liftIO $ compSrcExecute src req
          let retVal =
                do
                  mapM_ (dependOn . wrapCompSrcDepWithId (Proxy @s) cid) inputs
                  return res
          return retVal
    reg <- CompEngineM (asks (ce_compFlowRegistry . ce_compEngineIfs))
    action <-
      withTypedCompSrcId reg srcId srcFun >>= \case
        Ok x -> pure x
        Fail reason ->
          let msg = "Refusing to run request for data source " ++ show srcId ++ ": " ++ reason
           in pure (return (Fail msg))
    loop env (action >>= contToCompM . cont)

  -- The "Value" helpers below are the CompReqCombined leaf-preparation
  -- counterparts of doAnyEvalReq/doAnyCacheReq/doCompSinkReq/doCompSrcReq
  -- just above: same bodies, minus the trailing `loop env (... cont)` that
  -- resumes a specific suspended caller, since a leaf being prepared inside
  -- a batch doesn't have one of its own yet -- the whole batch shares a
  -- single continuation, applied once after every leaf's Prep has run (see
  -- prepLeaf and the CompReqCombined case below). None of the original four
  -- doAnyEvalReq/doAnyCacheReq/doCompSinkReq/doCompSrcReq change: this is
  -- new code alongside them, not a rewrite of the overwhelmingly common
  -- single-leaf path. The source leaf's counterpart is prepSrcLeaf, not a
  -- plain "Value" function of this shape: unlike the other three it also
  -- has to decide, using the registry lookup it needs anyway, whether this
  -- leaf's compSrcExecute runs inline or as a queued job -- see its own
  -- haddock.

  doAnyEvalReqValue
    :: forall x
     . IsCompResult x
    => CompAp x
    -> CompEngineM (Maybe (CompApResult x))
  doAnyEvalReqValue innerCap = evalWithCacheValue False innerCap (doCompAp innerCap)

  doAnyCacheReqValue
    :: forall x
     . IsCompResult x
    => CompAp x
    -> CompEngineM (Maybe x)
  doAnyCacheReqValue innerCap =
    fmap (fmap cr_returnValue) (evalWithCacheValue True innerCap (return Nothing))

  doCompSinkReqValue
    :: forall a s
     . CompSink s
    => CompMEnv
    -> TypedCompSinkId s
    -> CompSinkReq s a
    -> CompEngineM (CompM (Fail a))
  doCompSinkReqValue env sinkId req = do
    let sinkFun sink = do
          (outputs, res) <- liftIO $ compSinkExecute sink req
          return (wrapCompSinkOuts sink outputs, pure res)
    reg <- CompEngineM (asks (ce_compFlowRegistry . ce_compEngineIfs))
    (outputs, action) <-
      withTypedCompSinkId reg sinkId sinkFun >>= \case
        Ok y -> pure y
        Fail reason ->
          let msg = "Refusing to run request for data sink " ++ show sinkId ++ ": " ++ reason
           in pure (emptyAnyCompOutSinksMap, return (Fail msg))
    tellOutputs (AnyCompAp outerCap) outputs
    liftIO (modifyIORef' (cme_outputs env) (<> outputs))
    pure action

  {- | The 'CompReqLeaf' counterpart of 'prepLeaf'\'s other three branches,
   but for 'CompLeafSrc': resolves this leaf's instance (one registry
   lookup), files this leaf's request into that instance's 'SrcGroup' (see
   'getOrCreateGroup' -- every leaf of the batch resolving to the same
   instance ends up in the same group, however many there turn out to be),
   and returns a 'CompEngineM' action that, when the batch's engine phase
   reaches it, makes sure the group's bundled call has happened (triggering
   it there and then if nothing has already -- see 'runGroupOnce') and then
   reads this leaf's own result back out of its slot.

   Which requests actually become one 'compSrcExecuteBatch' call, and
   whether that call is proactively dispatched to a worker or left to run
   lazily on the engine thread, is decided once per *group*, not per leaf --
   see the 'CompReqCombined' case of 'doSuspended', which is the only
   caller and is where @groupsRef@ comes from.
  -}
  prepSrcLeaf
    :: forall s a
     . CompSrc s
    => CompFlowRegistry
    -> IORef [(CompSrcInstIx, SomeSrcGroup)]
    -> TypedCompSrcId s
    -> CompSrcReq s a
    -> Prep (Fail a)
  prepSrcLeaf reg groupsRef sid req =
    Prep $
      withTypedCompSrcIdIndexed reg sid (\ix src -> pure (ix, src, compSrcConcurrency src)) >>= \case
        Fail reason ->
          let msg = "Refusing to run request for data source " ++ show sid ++ ": " ++ reason
           in pure (pure (return (Fail msg)))
        Ok (ix, src, conc) -> do
          slot <- newIORef Nothing
          g <- getOrCreateGroup groupsRef ix src conc
          modifyIORef' (sg_fetches g) (\fs -> fs . (SrcFetch req (writeIORef slot . Just) :))
          pure (readMySlot g slot)
   where
    -- NOTE: sid is this leaf's own id, already resolved via the registry
    -- lookup above -- every leaf that lands in the same group (same ix)
    -- necessarily resolved through the same key (see getOrCreateGroup's
    -- haddock on the ix<->CompSrcId correspondence), so this is exactly
    -- sg_src g's own compSrcId, computed once here instead of once per
    -- dependency inside readMySlot below (see wrapCompSrcDep's haddock).
    cid = unTypedCompSrcId sid
    -- The engine-thread half of every source leaf, job or not: make sure
    -- this leaf's group has actually run (a no-op if some other leaf's
    -- CompEngineM action, or a dispatched job, already triggered it -- see
    -- 'runGroupOnce'), then read this leaf's own slot and either re-raise a
    -- stored exception or finish exactly like before batching existed, via
    -- 'dependOn'. Every leaf's CompEngineM action here is composed
    -- leaf-by-leaf through CompEngineM's left-to-right Applicative (see
    -- Prep's haddock), so an exception raised for a leaf earlier in the
    -- batch stops any later leaf's slot from ever being read -- the
    -- leftmost failing leaf is the one whose exception escapes, mirroring
    -- compMAp's left-error bias at the Fail level (Types.hs, compMAp's
    -- haddock).
    readMySlot
      :: SrcGroup s
      -> IORef (Maybe (Either SomeException (CompSrcDeps s, Fail a)))
      -> CompEngineM (CompM (Fail a))
    readMySlot g slot = do
      liftIO (runGroupOnce g)
      liftIO (readIORef slot) >>= \case
        Nothing ->
          error "prepSrcLeaf: slot read before its group's compSrcExecuteBatch call finished -- invariant broken"
        Just (Left ex) -> liftIO (throwIO ex)
        Just (Right (inputs, res)) ->
          pure $
            do
              mapM_ (dependOn . wrapCompSrcDepWithId (Proxy @s) cid) inputs
              return res

  isCompReqCombined :: forall r. CompReq r -> Bool
  isCompReqCombined CompReqCombined{} = True
  isCompReqCombined _ = False

  -- | Whether @reqA@ and @reqB@ are both single source-read leaves against
  -- the *same* registered instance -- checkable without any registry
  -- lookup, since a 'TypedCompSrcId' already carries the untyped
  -- 'CompSrcId' the registry itself keys on (see 'unTypedCompSrcId'). Used
  -- only to keep the two-leaf fast path below from bypassing bundling for
  -- exactly the shape bundling exists for; see its call site.
  sameSrcInstance :: forall r1 r2. CompReq r1 -> CompReq r2 -> Bool
  sameSrcInstance (CompReqFlow (CompFlowReqSrc sidA _)) (CompReqFlow (CompFlowReqSrc sidB _)) =
    unTypedCompSrcId sidA == unTypedCompSrcId sidB
  sameSrcInstance _ _ = False

  {- | Prepare one leaf of a 'CompReqCombined' batch as a 'Prep' value.
   'CompLeafSrc' is the only leaf kind that ever joins a 'SrcGroup'; every
   other kind always runs inline, exactly as before batching existed.
  -}
  prepLeaf
    :: forall r
     . CompMEnv
    -> CompFlowRegistry
    -> IORef [(CompSrcInstIx, SomeSrcGroup)]
    -> CompReqLeaf r
    -> Prep r
  prepLeaf env reg groupsRef leaf =
    case leaf of
      CompLeafEval cap -> Prep (pure (pure <$> doAnyEvalReqValue cap))
      CompLeafCache cap -> Prep (pure (pure <$> doAnyCacheReqValue cap))
      CompLeafSrc sid req -> prepSrcLeaf reg groupsRef sid req
      CompLeafSink sid req -> Prep (pure (doCompSinkReqValue env sid req))

  doSuspended
    :: forall x a
     . CompMEnv
    -> CompReq a
    -> CompCont a x
    -> CompEngineM (CompResult x)
  doSuspended env req cont =
    case req of
      CompReqFlow (CompFlowReqSrc src req) -> doCompSrcReq env src req cont
      CompReqFlow (CompFlowReqSink sink req) -> doCompSinkReq env sink req cont
      CompReqEval compAp -> doAnyEvalReq env compAp cont
      CompReqCache compAp -> doAnyCacheReq env compAp cont
      CompReqCombined reqA reqB -> do
        reg <- CompEngineM (asks (ce_compFlowRegistry . ce_compEngineIfs))
        width <- liftIO (readCompFlowConcurrency reg)
        if not (isCompReqCombined reqA)
          && not (isCompReqCombined reqB)
          && unCompFlowConcurrency width == 1
          && not (sameSrcInstance reqA reqB)
          then do
            -- Fast path for two *leaves* combined directly at width 1 (e.g.
            -- plain `f <$> a <*> b`, the overwhelmingly common shape a batch
            -- of exactly two suspended actions takes) -- today's pre-Prep
            -- code, kept verbatim rather than routed through
            -- traverseCompReq/Prep. Also excluded here: two source leaves
            -- against the *same* instance, even at width 1 -- bundling them
            -- into one compSrcExecuteBatch call needs no worker thread (see
            -- "Control.Computations.CompEngine.CompSrc"'s
            -- compSrcExecuteBatch haddock), so this specific two-leaf shape
            -- must still fall through to the general path below to get that
            -- benefit; every other two-leaf shape keeps taking this
            -- shortcut. Measured: at ~1.1M cap evaluations, going through
            -- Prep for the general two-leaf shape cost a small but real
            -- (~1.5-2%, beyond run-to-run noise) cold-eval wall-time
            -- regression for no change in max_live_bytes, so it isn't worth
            -- paying except where bundling actually pays for it. Above
            -- width 1, a two-leaf batch instead falls through to the
            -- general path below and goes through Prep like everything
            -- else, so its source leaves can actually be queued and
            -- overlap.
            -- Both branches share `env`, so whatever either records via
            -- tellDep lands directly in the shared accumulator; no manual
            -- union of per-branch dependency sets is needed.
            resA <- doSuspended env reqA return
            resB <- doSuspended env reqB return
            let resCont =
                  do
                    res <- (,) <$> compMFinished resA <*> compMFinished resB
                    contToCompM (cont res)
            loop env resCont
          else do
            -- General path: batches with more than two leaves (deeper
            -- CompReqCombined nesting), a two-leaf batch at width > 1, or a
            -- two-leaf batch of same-instance source reads at any width --
            -- flatten the whole thing to its leaves in one traversal (see
            -- traverseCompReq and Prep) instead of recursing node by node.
            -- groupsRef is fresh per batch: every CompLeafSrc leaf reached
            -- during the traversal below either creates or joins a
            -- 'SrcGroup' in it, keyed by resolved instance (see
            -- prepSrcLeaf/getOrCreateGroup); by the time the traversal's IO
            -- layer finishes, every group holds its full, final fetch list.
            groupsRef <- liftIO (newIORef [])
            enginePhase <- liftIO (unPrep (traverseCompReq (prepLeaf env reg groupsRef) req))
            groups <- liftIO (map snd <$> readIORef groupsRef)
            -- Proactively dispatch only the groups that can genuinely
            -- overlap something: FlowConcurrent instances, width > 1, and
            -- (as ever) only when the RTS can actually run threads
            -- concurrently. Every other group -- FlowSerial, or any
            -- instance at width 1 -- is left untriggered here: no job, no
            -- worker thread, nothing. It runs lazily instead, the first
            -- time enginePhase's left-to-right walk reaches one of its
            -- member leaves (see runGroupOnce), which is exactly the
            -- position an unbundled single call would have run at -- the
            -- mechanism that keeps this dispatch-then-drain step from
            -- moving a FlowSerial/width-1 group's actual round trip earlier
            -- than the golden ordering trace in TestCompReqCombined allows.
            --
            -- Dispatch-then-drain, never overlapping dispatchJobs with the
            -- engine phase that follows it, for three reasons:
            --  * allCompSrcChanges (CompFlowRegistry.hs) folds every
            --    source's compSrcWaitChanges into one STM transaction on
            --    the engine thread; it must never race a source's
            --    concurrently-running compSrcExecuteBatch, and draining
            --    first guarantees that;
            --  * a nested batch (e.g. an eval leaf whose body issues its
            --    own wide batch) can't starve this pool: by the time
            --    enginePhase runs (and could recurse into doSuspended
            --    again), every worker from *this* dispatchJobs call has
            --    already exited, so no outer worker is still holding a
            --    pool slot;
            --  * enginePhase's read of each dispatched group's member slots
            --    (see prepSrcLeaf's readMySlot) can then never block -- the
            --    job that fills them has unconditionally already finished.
            -- CompEngineM's Applicative still sequences every (now
            -- possibly-slot-reading) leaf's engine-thread effects strictly
            -- left to right (see its instance above), which is what keeps
            -- leaf order identical to the recursive walk above for
            -- everything except which source *groups* ran concurrently
            -- during the dispatch phase (see the golden trace in
            -- TestCompReqCombined, and its width-8 companion assertion
            -- that only *src* trace entries may float).
            let jobs =
                  [ runGroupOnce g
                  | SomeSrcGroup g <- groups
                  , rtsSupportsBoundThreads
                  , unCompFlowConcurrency width > 1
                  , sg_conc g == FlowConcurrent
                  ]
            liftIO (dispatchJobs (unCompFlowConcurrency width) jobs)
            inner <- enginePhase
            loop env (inner >>= contToCompM . cont)

  loop
    :: forall x
     . CompMEnv
    -> CompM x
    -> CompEngineM (CompResult x)
  loop env gen = do
    yield <- liftIO (runCompM gen env)
    case yield of
      CompFinished finalResult -> return finalResult
      CompSuspended req cont -> doSuspended env req cont

execAp :: AnyCompAp -> CompEngineM ()
execAp (AnyCompAp cap) = void $ doCompAp cap

initCompEngine :: CompEngineIfs -> IO CompEngine
initCompEngine compEngineIfs = do
  counter <- newIORef 0
  return (CompEngine compEngineIfs counter)

startCompEngine
  :: (F.Foldable t)
  => CompEngineIfs
  -> t AnyCompAp
  -> IO CompEngine
startCompEngine compEngineIfs genAps =
  do
    compEngine <- initCompEngine compEngineIfs
    logNote "Starting CompEngine"
    (_, g) <- runCompEngineM' (F.mapM_ execAp genAps) compEngine
    when (garbage g /= mempty) $
      do
        logError "Garbage generated while starting CompEngine.  This should not happen."
        logError "Not collecting this garbage.  The world is a dirty place:"
    mapM_ (logNote . ("- " ++) . show) (garbage_caps (garbage g))
    mapM_
      (logNote . ("- " ++) . show)
      (fmap (map fst . anyOutsMapToList) $ garbage_outputs $ garbage g)
    logNote "CompEngine started."
    return compEngine

evalWithCompEngine
  :: IsCompResult r
  => CompEngine
  -> CompAp r
  -> IO (Maybe r)
evalWithCompEngine compEngine genAp =
  evalCompEngineM (liftM (fmap cr_returnValue) (doCompAp genAp)) compEngine

notifyCompEngine
  :: CompEngine
  -> [AnyCompSrcDep]
  -> IO EnqueueInfo
notifyCompEngine compEngine reqDeps =
  flip evalCompEngineM compEngine $
    withCompState (\sif -> enqueueStaleCaps sif deps)
 where
  deps = map CompEngDepSrc reqDeps

stepCompEngine
  :: CompEngine
  -> GenDel
  -> IO (Int, GenDel)
  -- ^ approximate number of computations that still need to be run
stepCompEngine compEngine g =
  runCompEngineM action compEngine g
 where
  action =
    do
      mCap <- withCompState dequeueNextCap
      case mCap of
        Just cap ->
          do
            execAp cap
            withCompState staleQueueSize
        Nothing -> return (-1)

stopCompEngine :: CompEngine -> IO ()
stopCompEngine compEngine =
  flip evalCompEngineM compEngine $
    logNote "CompEngine stopped."
