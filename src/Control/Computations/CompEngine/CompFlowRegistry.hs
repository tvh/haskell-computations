{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Control.Computations.CompEngine.CompFlowRegistry (
  CompFlowRegistry,
  newCompFlowRegistry,
  CompFlowConcurrency,
  mkCompFlowConcurrency,
  unCompFlowConcurrency,
  setCompFlowConcurrency,
  readCompFlowConcurrency,
  withCompSinkId,
  withTypedCompSinkId,
  withTypedCompSrcId,
  forAllSrcs_,
  forAllSinks_,
  registerCompSrc,
  unregisterCompSrc,
  registerCompSink,
  unregisterCompSink,
  allCompSrcChanges,
  Blocking (..),
)
where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.Utils.Fail
import Control.Computations.Utils.Logging

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Proxy
import Data.Typeable

data RegState = RegState
  { rs_srcs :: HashMap CompSrcId AnyCompSrc
  , rs_sinks :: HashMap CompSinkId AnyCompSink
  }

{- | Width of the fixed worker pool a wide 'CompReqCombined' batch (see
 "Control.Computations.CompEngine.Types") may dispatch 'FlowConcurrent'
 source leaves to. 1 (the default -- see 'newCompFlowRegistry') means no
 worker threads at all: every leaf runs inline, in leaf order, on the
 engine thread, exactly as before this knob existed.

 Deliberately just an 'Int' wrapper, not e.g. a number-of-capabilities
 lookup or anything more elaborate: how wide to run is a property of the
 workload (how much genuinely-concurrent I\/O it has, how contended the
 sources are), not of the machine, and is exactly as easy to get wrong in
 either direction, so this leaves the choice entirely to the caller.
-}
newtype CompFlowConcurrency = CompFlowConcurrency {unCompFlowConcurrency :: Int}
  deriving (Eq, Show)

{- | Clamp to at least 1. A width of 0 (or negative) would mean the fixed
 worker pool never runs any queued job at all -- silently dropping source
 leaves rather than executing them -- so this floors at the smallest
 meaningful width instead of letting a caller construct that footgun.
-}
mkCompFlowConcurrency :: Int -> CompFlowConcurrency
mkCompFlowConcurrency = CompFlowConcurrency . max 1

{- | The set of sources and sinks currently available to comp bodies, plus
 the 'CompFlowConcurrency' width new work should be prepared against (see
 'setCompFlowConcurrency'\/'readCompFlowConcurrency'). Create one with
 'newCompFlowRegistry', populate it with 'registerCompSrc'\/
 'registerCompSink' (or the 'Control.Computations.CompEngine.Driver.regSrc'\/
 'Control.Computations.CompEngine.Driver.regSink' helpers), and pass it to
 'Control.Computations.CompEngine.Driver.compDriver'.

 The concurrency width lives here rather than on 'Control.Computations.CompEngine.Core.CompEngineIfs'
 or 'Control.Computations.CompEngine.Run.RunCompEngineIf' for two reasons,
 both about not touching signatures that have nothing to do with it:

  * "Control.Computations.CompEngine.Impl" already has a 'CompFlowRegistry'
    in hand at every flow request site, via
    @asks (ce_compFlowRegistry . ce_compEngineIfs)@ -- so putting the width
    here means no other signature in the engine (not 'CompEngineIfs', not
    'Control.Computations.CompEngine.Run.RunCompEngineIf', not
    'Control.Computations.CompEngine.Driver.compDriver'\'s own type) needs
    to change to thread it through;
  * 'Control.Computations.CompEngine.Driver.compDriver'\/'compDriver'' already
    construct a fresh registry and hand it to the caller's
    @withRegisteredFlows@ callback before running anything (see
    "Control.Computations.CompEngine.Driver", the @reg <- newCompFlowRegistry@
    line just above that hand-off) -- so a benchmark or test can call
    'setCompFlowConcurrency' on that same registry, from that same callback,
    entirely without forking the driver or adding a parameter to it.
-}
data CompFlowRegistry = CompFlowRegistry
  { cfr_state :: TVar RegState
  , cfr_concurrency :: TVar CompFlowConcurrency
  }

newCompFlowRegistry :: IO CompFlowRegistry
newCompFlowRegistry = do
  let state = RegState HashMap.empty HashMap.empty
  v <- newTVarIO state
  concVar <- newTVarIO (mkCompFlowConcurrency 1)
  pure (CompFlowRegistry v concVar)

{- | Set the width of @reg@'s fixed worker pool for source leaves prepared
 from now on (see 'CompFlowConcurrency'). Takes effect for the next
 'CompReqCombined' batch prepared against this registry -- a batch already
 in flight keeps whatever width it read when it started.

 __Honest behavioural difference at width > 1__: if one source leaf in a
 batch throws, every other leaf's 'Control.Computations.CompEngine.CompSrc.compSrcExecute'
 has still run by the time that exception is observed -- all queued jobs are
 dispatched and drained together, before any of their results are inspected
 (see "Control.Computations.CompEngine.Impl"'s @Prep@\/@SrcJobs@). At the
 'Control.Computations.Utils.Fail.Fail' level this already matches today's
 behaviour, because 'Control.Computations.CompEngine.Types.compMAp' runs
 both sides of every applicative combination unconditionally, by design (see
 its haddock). At the *exception* level -- a genuine Haskell exception
 escaping 'compSrcExecute', as opposed to a 'Control.Computations.Utils.Fail.Fail'
 value it returns -- this is new: at width 1 no worker pool exists, so a
 throwing leaf aborts the batch immediately and no leaf after it ever runs.
 Both cases still surface only the left(most)-failing leaf's exception to
 the caller, matching 'compMAp'\'s left-error bias (see
 "Control.Computations.CompEngine.Impl"'s @readJobSlot@).
-}
setCompFlowConcurrency :: CompFlowRegistry -> CompFlowConcurrency -> IO ()
setCompFlowConcurrency reg = atomically . writeTVar (cfr_concurrency reg)

readCompFlowConcurrency :: CompFlowRegistry -> IO CompFlowConcurrency
readCompFlowConcurrency = readTVarIO . cfr_concurrency

withTypedCompSrcId
  :: forall m a s
   . (MonadIO m, CompSrc s)
  => CompFlowRegistry
  -> TypedCompSrcId s
  -> (s -> m a)
  -> m (Fail a)
withTypedCompSrcId reg typedKey fun =
  withCompSrcId reg (unTypedCompSrcId typedKey) fun

withCompSrcId
  :: forall m a s
   . (MonadIO m, CompSrc s)
  => CompFlowRegistry
  -> CompSrcId
  -> (s -> m a)
  -> m (Fail a)
withCompSrcId reg key fun = do
  regState <- liftIO $ readTVarIO (cfr_state reg)
  case Map.lookup key (rs_srcs regState) of
    Just (AnyCompSrc src) ->
      case cast src of
        Just src' -> Ok <$> fun src'
        Nothing ->
          pure $
            Fail
              ( "Expected CompSrc of type "
                  ++ show (typeRep (Proxy :: Proxy s))
                  ++ " but got: "
                  ++ show (compSrcId src)
              )
    Nothing ->
      do
        let msg = "No CompSrc registered for key " ++ show key
        logWarn msg
        pure $ Fail msg

withTypedCompSinkId
  :: forall m a s
   . (MonadIO m, CompSink s)
  => CompFlowRegistry
  -> TypedCompSinkId s
  -> (s -> m a)
  -> m (Fail a)
withTypedCompSinkId reg typedKey fun =
  withCompSinkId reg (unTypedCompSinkId typedKey) fun

withCompSinkId
  :: forall m a s
   . (MonadIO m, CompSink s)
  => CompFlowRegistry
  -> CompSinkId
  -> (s -> m a)
  -> m (Fail a)
withCompSinkId reg key fun = do
  regState <- liftIO $ readTVarIO (cfr_state reg)
  case Map.lookup key (rs_sinks regState) of
    Just (AnyCompSink sink) ->
      case cast sink of
        Just sink' -> Ok <$> fun sink'
        Nothing ->
          pure $
            Fail
              ( "Expected CompSink of type "
                  ++ show (typeRep (Proxy :: Proxy s))
                  ++ " but got: "
                  ++ show (compSinkId sink)
              )
    Nothing ->
      do
        let msg = "No CompSink registered for key " ++ show key
        logWarn msg
        pure $ Fail msg

forAllSrcs_
  :: MonadIO m
  => CompFlowRegistry
  -> (forall s. CompSrc s => s -> m ())
  -> m ()
forAllSrcs_ reg srcFun =
  do
    regState <- liftIO $ readTVarIO (cfr_state reg)
    F.forM_ (rs_srcs regState) $ \(AnyCompSrc src) -> srcFun src

forAllSinks_
  :: MonadIO m
  => CompFlowRegistry
  -> (forall s. CompSink s => s -> m ())
  -> m ()
forAllSinks_ reg sinkFun =
  do
    regState <- liftIO $ readTVarIO (cfr_state reg)
    F.forM_ (rs_sinks regState) $ \(AnyCompSink sink) -> sinkFun sink

registerCompSrc :: CompSrc s => CompFlowRegistry -> s -> IO ()
registerCompSrc reg src =
  do
    logInfo ("Registering " ++ show (compSrcId src))
    atomically $ modifyTVar' (cfr_state reg) $ \state ->
      state{rs_srcs = HashMap.insert (compSrcId src) (AnyCompSrc src) (rs_srcs state)}

unregisterCompSrc :: CompSrc s => CompFlowRegistry -> s -> IO ()
unregisterCompSrc reg src =
  do
    logInfo ("Unregistering " ++ show (compSrcId src))
    atomically $ modifyTVar' (cfr_state reg) $ \state ->
      state{rs_srcs = HashMap.delete (compSrcId src) (rs_srcs state)}

-- | Make a sink available to comp bodies via 'Control.Computations.CompEngine.Types.compSinkReq'.
registerCompSink :: CompSink s => CompFlowRegistry -> s -> IO ()
registerCompSink reg sink =
  do
    logInfo ("Registering " ++ show (compSinkId sink))
    atomically $ modifyTVar' (cfr_state reg) $ \state ->
      state{rs_sinks = HashMap.insert (compSinkId sink) (AnyCompSink sink) (rs_sinks state)}

unregisterCompSink :: CompSink s => CompFlowRegistry -> s -> IO ()
unregisterCompSink reg sink =
  do
    logInfo ("Unregistering " ++ show (compSinkId sink))
    atomically $ modifyTVar' (cfr_state reg) $ \state ->
      state{rs_sinks = HashMap.delete (compSinkId sink) (rs_sinks state)}

data Blocking = Block | DontBlock
  deriving (Eq, Show)

allCompSrcChanges :: CompFlowRegistry -> Blocking -> STM (HashSet AnyCompSrcDep)
allCompSrcChanges reg b =
  do
    regState <- readTVar (cfr_state reg)
    let srcs = rs_srcs regState
    logTraceSTM ("Waiting for changes on " ++ show (Map.size srcs) ++ " sources")
    let changesActions :: [STM (HashSet AnyCompSrcDep)]
        changesActions = map (\(AnyCompSrc src) -> getChanges src) (HashMap.elems srcs)
    -- wait for at least one change
    set1 <-
      case b of
        Block -> F.foldl' orElse retry changesActions
        DontBlock -> pure HashSet.empty
    -- collect all changes
    sets <- mapM (`orElse` pure HashSet.empty) changesActions
    pure (HashSet.unions (set1 : sets))
 where
  getChanges :: CompSrc s => s -> STM (HashSet AnyCompSrcDep)
  getChanges s =
    do
      deps <- compSrcWaitChanges s
      when (HashSet.null deps) $
        logDebugSTM (show (compSrcId s) ++ " returned no results, it should have blocked in STM.")
      pure (HashSet.map (wrapCompSrcDep s) deps)
