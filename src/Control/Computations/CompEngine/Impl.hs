{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
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
 MonadIO)@ (via GeneralizedNewtypeDeriving): profiling after the CompM-over-
 IO rewrite (docs/benchmark-notes.md, Stage 2) found a live, allocating
 `$fMonadCompEngineM_$s$fMonadStateT_$c>>=` cost center on the suspend/
 resume hot path (`loop`/`doSuspended` now do a real monadic bind on every
 round trip, where the old pure representation didn't need one at all) --
 i.e. the derived instance's dictionary-passing wasn't fully specializing
 away even under -O2. Each method here is a one-line unwrap-delegate-
 rewrap around the identical underlying `StateT`/`ReaderT`/`IO` operation
 (so this is representationally a no-op, same as what deriving generated),
 but the explicit INLINE pragma gives GHC's simplifier a direct mandate to
 substitute the body at every call site and keep specializing through
 `mtl`'s own (also INLINE-marked) StateT/ReaderT instances down to concrete
 IO, rather than leaving a standalone dictionary method for the profiler
 (and the runtime) to keep finding.
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
                -- CompCacheMeta no longer pre-renders/stores a logrepr (see
                -- its haddock in Types.hs) -- render the already-in-scope
                -- typed value directly, on demand, only for this log line.
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
    withCompState (flip lookupCapResult cap)
      >>= withCapLookup cap f cont (evalWithCapCached cap f cont staleOk)
   where
    cont
      :: Maybe (CompApResult x)
      -> CompEngineM (CompResult a)
    cont x = loop env (contToCompM $ k x)

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
        -- TODO: Make this bit parrallel
        -- Both branches share `env`, so whatever either records via
        -- tellDep lands directly in the shared accumulator -- no manual
        -- union needed (contrast the old `tellDep (wA <> wB)` below it
        -- replaced).
        resA <- doSuspended env reqA return
        resB <- doSuspended env reqB return
        let resCont =
              do
                res <- (,) <$> compMFinished resA <*> compMFinished resB
                contToCompM (cont res)
        loop env resCont

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
