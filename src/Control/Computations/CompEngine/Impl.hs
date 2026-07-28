{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
import Control.Computations.Utils.Logging
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import Control.Monad
import Control.Monad.Reader
-- Strict, not the mtl default (Control.Monad.State re-exports the Lazy
-- variant) -- see CompEngineM's Monad instance below for why this matters
-- on the suspend/resume hot path.
import Control.Monad.State.Strict
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype CompEngine = CompEngine
  { ce_compEngineIfs :: CompEngineIfs
  }

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
  }
  deriving (Show, Eq)

instance Semigroup GenDel where
  (<>) (GenDel a1 b1) (GenDel a2 b2) =
    GenDel (Map.unionWith unionAnyCompSinkOutsMap a1 a2) (b1 <> b2)

instance Monoid GenDel where
  mempty = GenDel mempty mempty

generated :: AnyCompAp -> AnyCompSinkOutsMap -> GenDel
generated k mo = GenDel (Map.singleton k mo) mempty

deleted :: Garbage -> GenDel
deleted = GenDel mempty

garbage :: GenDel -> Garbage
garbage (GenDel gen (Garbage garbage_caps garbage_deps garbage_outputs)) =
  -- First delete any outputs from the generated outputs
  -- that belong to a garbage cap
  let genWithCapsDeleted =
        foldr Map.delete gen (HashSet.toList garbage_caps)
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
         in Garbage garbage_caps garbage_deps garbageOutputsTrimmed
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

{- | A unit of concurrent source work, queued by a leaf's 'Prep' rather than
 run inline. Nothing constructs one yet -- see 'Prep'\'s haddock. Kept as a
 real (if currently uninhabited-in-practice) type rather than deferred to a
 later commit so 'SrcJobs' and 'Prep'\'s shape can be built, compiled and
 exercised now, ahead of anything actually populating it.
-}
data SrcJob

{- | The jobs queued by one leaf-traversal, in the order they were queued.
 A plain @[SrcJob]@ built by repeated '<>' would make a left-nested '<*>'
 chain (thousands of leaves, e.g. the wide-batch case in
 "Control.Computations.CompEngine.Tests.TestCompReqCombined") quadratic:
 every leaf's singleton list gets re-copied by every append above it. The
 difference-list representation makes every '<>' an O(1) function
 composition, with the whole batch's list built (and traversed) exactly
 once, at the end.
-}
newtype SrcJobs = SrcJobs ([SrcJob] -> [SrcJob])

instance Semigroup SrcJobs where
  SrcJobs f <> SrcJobs g = SrcJobs (f . g)

instance Monoid SrcJobs where
  mempty = SrcJobs id

{- | The result of preparing one leaf of a 'CompReqCombined' batch (see
 'Control.Computations.CompEngine.Types.traverseCompReq'): what
 (if anything) it queues as concurrent work, plus the engine-thread
 computation that actually produces its value.

 Spelled out, this is @Compose IO (Compose ((,) SrcJobs) (Compose
 CompEngineM CompM))@ written by hand rather than pulled in from
 "Data.Functor.Compose" -- a composition of lawful 'Applicative's is itself
 a lawful 'Applicative' (that's the whole point of 'Compose'), so 'Prep'\'s
 own laws follow from its four layers' each being exactly what they claim:

  * the outer 'IO' layer is where a later commit's scheduler will decide,
    per leaf (via 'Control.Computations.CompEngine.CompSrc.compSrcConcurrency'),
    whether that leaf's work becomes a queued 'SrcJob' or still runs inline;
    in this commit it never does anything but 'pure' its result, so the two
    read identically here;
  * @(,) SrcJobs@ carries the (currently always-'mempty') jobs queued while
    preparing this leaf, combined across leaves via the 'SrcJobs' monoid;
  * 'CompEngineM' is the engine-thread computation that does this leaf's
    actual work -- cache lookups, 'compSrcExecute'\/'compSinkExecute' calls,
    recursive cap evaluation. Its 'Applicative' sequences effects strictly
    left to right (see its instance above), which is what keeps leaf order
    identical to the recursive walk 'doSuspended' used before this type
    existed -- the golden trace in
    "Control.Computations.CompEngine.Tests.TestCompReqCombined" is pinned on
    exactly that;
  * 'CompM' is the leaf's own deferred result. Combining two leaves'
    'CompM's uses 'CompM'\'s own '<*>' ('compMAp') rather than a hand-rolled
    combinator: reusing it is what preserves 'compMAp'\'s deliberate
    left-error bias and its "both sides always run" dependency-tracking
    guarantee (see its haddock) for a batch assembled this way, instead of
    quietly re-deciding those rules a second time here.
-}
newtype Prep a = Prep {unPrep :: IO (SrcJobs, CompEngineM (CompM a))}

instance Functor Prep where
  fmap f (Prep io) = Prep (fmap (\(jobs, m) -> (jobs, fmap (fmap f) m)) io)
  {-# INLINE fmap #-}

instance Applicative Prep where
  pure x = Prep (pure (mempty, pure (pure x)))
  {-# INLINE pure #-}
  Prep iof <*> Prep iox = Prep $ do
    (jobsF, mf) <- iof
    (jobsX, mx) <- iox
    -- The outer `<*>` is CompEngineM's (left-to-right effect sequencing);
    -- the inner one, fmapped in, is CompM's own `<*>` -- i.e. `compMAp`.
    pure (jobsF <> jobsX, (<*>) <$> mf <*> mx)
  {-# INLINE (<*>) #-}

{- | Here we remove the cap that generated the garbage from the
 total garbage cap set in the state
 This is necessary to avoid later marking output as garbage
 which might have been regenerated by this cap
 see test_dontDeleteAfterRevival in TestRevive
-}
tellGarbage :: Maybe AnyCompAp -> Garbage -> CompEngineM ()
tellGarbage mKey g =
  CompEngineM $
    do
      let removeCapFromGarbage genDel =
            let appended@(GenDel _ garb@(Garbage gCaps _ _)) =
                  genDel `mappend` deleted g
             in case mKey of
                  Nothing ->
                    appended
                  Just key ->
                    let gCaps' = HashSet.delete key gCaps
                     in appended{_gd_garbage = garb{garbage_caps = gCaps'}}
      modify' removeCapFromGarbage
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
    (deps, res) <- evalCompAp gap
    maybeRes <-
      case res of
        CompResultFail msg -> logNote ("Cap " ++ show gap ++ " failed: " ++ msg) >> return Nothing
        CompResultOk ok ->
          do
            logDebug ("Cap " ++ show gap ++ " succeeded.")
            return (Just ok)
    (staleCaps, gcCaps) <-
      withCompState $ \sif ->
        capEvaluationFinished sif gap deps (fmap cr_returnValue maybeRes)
    tellGarbage (maybeRes >> Just (AnyCompAp gap)) gcCaps
    logStale ("Eval of " ++ show gap) staleCaps
    return maybeRes

evalCompAp
  :: forall a
   . IsCompResult a
  => CompAp a
  -> CompEngineM (DepSet, CompResult (CompApResult a))
evalCompAp outerCap =
  do
    logDebug ("Evaluating " ++ show outerCap)
    -- Fresh per-cap-evaluation dependency accumulator (see CompMEnv's
    -- haddock in Types.hs). `env` is threaded explicitly through every
    -- helper below rather than captured via a Reader in CompEngineM: its
    -- lifetime is scoped to exactly this one evaluation, and explicit
    -- threading makes that scoping visible instead of relying on someone
    -- remembering to reset a Reader-carried ref between nested cap evals.
    depsRef <- liftIO (newIORef [])
    let env = CompMEnv{cme_compMap = r, cme_deps = depsRef}
    finalResult <- loop env (initCompAp outerCap)
    accumulated <- liftIO (readIORef depsRef)
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
    return (deps, ev)
 where
  r =
    case outerCap of
      CompAp _ comp _ -> comp_compMap comp
  doAnyEvalReq
    :: forall a x
     . IsCompResult x
    => CompMEnv
    -> CompAp x
    -> CompCont (Maybe (CompApResult x)) a
    -> CompEngineM (CompResult a)
  doAnyEvalReq env innerCap k =
    evalWithCache env False innerCap k (doCompAp innerCap)

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
    withCompState (\sif -> trackOutput sif outerCap outputs)
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
    let srcFun src = do
          (inputs, res) <- liftIO $ compSrcExecute src req
          let retVal =
                do
                  mapM_ (dependOn . wrapCompSrcDep src) inputs
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

  -- The four "Value" helpers below are the CompReqCombined leaf-preparation
  -- counterparts of doAnyEvalReq/doAnyCacheReq/doCompSinkReq/doCompSrcReq
  -- just above: same bodies, minus the trailing `loop env (... cont)` that
  -- resumes a specific suspended caller, since a leaf being prepared inside
  -- a batch doesn't have one of its own yet -- the whole batch shares a
  -- single continuation, applied once after every leaf's Prep has run (see
  -- prepLeaf and the CompReqCombined case below). None of the original four
  -- change: this is new code alongside them, not a rewrite of the
  -- overwhelmingly common single-leaf path.

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
    => TypedCompSinkId s
    -> CompSinkReq s a
    -> CompEngineM (CompM (Fail a))
  doCompSinkReqValue sinkId req = do
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
    withCompState (\sif -> trackOutput sif outerCap outputs)
    pure action

  doCompSrcReqValue
    :: forall a s
     . CompSrc s
    => TypedCompSrcId s
    -> CompSrcReq s a
    -> CompEngineM (CompM (Fail a))
  doCompSrcReqValue srcId req = do
    let srcFun src = do
          (inputs, res) <- liftIO $ compSrcExecute src req
          let retVal =
                do
                  mapM_ (dependOn . wrapCompSrcDep src) inputs
                  return res
          return retVal
    reg <- CompEngineM (asks (ce_compFlowRegistry . ce_compEngineIfs))
    withTypedCompSrcId reg srcId srcFun >>= \case
      Ok x -> pure x
      Fail reason ->
        let msg = "Refusing to run request for data source " ++ show srcId ++ ": " ++ reason
         in pure (return (Fail msg))

  isCompReqCombined :: forall r. CompReq r -> Bool
  isCompReqCombined CompReqCombined{} = True
  isCompReqCombined _ = False

  {- | Prepare one leaf of a 'CompReqCombined' batch as a 'Prep' value. In
   this commit every branch queues no 'SrcJob' (the 'mempty') and just
   'pure's its own already-built 'CompEngineM' action -- no leaf becomes
   concurrent work yet, this only isolates leaf preparation from the
   continuation-passing 'doSuspended' otherwise uses, ahead of a later
   commit teaching the 'IO' layer to actually queue something.

   Takes the width read from the registry (see 'doSuspended's general path
   below) as an explicit argument, unused for now, rather than have each
   branch read it independently: a future commit's per-source-leaf
   job-or-inline decision needs it, and threading it through here now means
   that decision won't need a new plumbing path of its own later.
  -}
  prepLeaf :: forall r. CompFlowConcurrency -> CompReqLeaf r -> Prep r
  prepLeaf _width leaf =
    case leaf of
      CompLeafEval cap -> Prep (pure (mempty, pure <$> doAnyEvalReqValue cap))
      CompLeafCache cap -> Prep (pure (mempty, pure <$> doAnyCacheReqValue cap))
      CompLeafSrc sid req -> Prep (pure (mempty, doCompSrcReqValue sid req))
      CompLeafSink sid req -> Prep (pure (mempty, doCompSinkReqValue sid req))

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
      CompReqCombined reqA reqB
        | not (isCompReqCombined reqA), not (isCompReqCombined reqB) -> do
            -- Fast path for two *leaves* combined directly (e.g. plain
            -- `f <$> a <*> b`, the overwhelmingly common shape a batch of
            -- exactly two suspended actions takes) -- today's pre-Prep code,
            -- kept verbatim rather than routed through traverseCompReq/Prep.
            -- Measured: at ~1.1M cap evaluations, going through Prep even
            -- for this shape cost a small but real (~1.5-2%, beyond
            -- run-to-run noise) cold-eval wall-time regression for no
            -- change in max_live_bytes, so it isn't worth paying here.
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
        | otherwise -> do
            -- General path, for batches with more than two leaves (deeper
            -- CompReqCombined nesting): flatten the whole thing to its
            -- leaves in one traversal (see traverseCompReq and Prep)
            -- instead of recursing node by node. Still fully sequential:
            -- Prep's IO layer never queues a job in this commit, and
            -- CompEngineM's Applicative sequences every leaf's
            -- engine-thread effects strictly left to right, so this
            -- preserves the exact leaf order the recursive walk above has
            -- (see the golden trace in TestCompReqCombined) -- concurrency
            -- is future work, not this commit's.
            --
            -- The width read below is likewise unused for now (see
            -- prepLeaf's haddock) -- reading it here, once per batch,
            -- ahead of the traversal is where a later commit needs it to
            -- already be, so this commit wires that read in without
            -- changing behaviour.
            reg <- CompEngineM (asks (ce_compFlowRegistry . ce_compEngineIfs))
            width <- liftIO (readCompFlowConcurrency reg)
            (_jobs, enginePhase) <- liftIO (unPrep (traverseCompReq (prepLeaf width) req))
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
initCompEngine compEngineIfs = return (CompEngine compEngineIfs)

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
