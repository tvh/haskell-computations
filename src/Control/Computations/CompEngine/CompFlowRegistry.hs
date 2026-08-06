{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Control.Computations.CompEngine.CompFlowRegistry (
  CompFlowRegistry,
  newCompFlowRegistry,
  CompEvalConcurrency,
  mkCompEvalConcurrency,
  unCompEvalConcurrency,
  setCompEvalConcurrency,
  readCompEvalConcurrency,
  withCompSinkId,
  withTypedCompSinkId,
  withTypedCompSrcId,
  CompSrcInstIx,
  withTypedCompSrcIdIndexed,
  forAllSrcs_,
  forAllSinks_,
  registerCompSrc,
  unregisterCompSrc,
  registerCompSink,
  unregisterCompSink,
  allCompSrcChanges,
  Blocking (..),
  lookupSrcLock,
  withSinkInstLock,
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

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (..))
import Data.Proxy
import Data.Typeable

{- | A dense index assigned to a 'CompSrc' instance the moment it's
 registered (see 'registerCompSrc'), monotonically increasing and never
 reused, even across an unregister\/re-register cycle. Exists purely so
 "Control.Computations.CompEngine.Impl"'s 'CompReqCombined' batch
 preparation -- which buckets a batch's source leaves by the instance they
 resolve to, and in practice sees only 1 to 5 distinct instances per batch,
 however many thousand leaves -- can key its per-batch lookup table on a
 cheap 'Int' comparison instead of hashing a 'CompSrcId' (two
 'Data.Text.Text' values, hashed through its Generic-derived instance) on
 every source leaf. Never exported from the public
 "Control.Computations.CompEngine" facade (see that module's import of this
 one) -- interning stays an implementation detail of this module and
 "Control.Computations.CompEngine.Impl", not something a 'CompSrc' instance
 or its caller ever needs to know exists.
-}
newtype CompSrcInstIx = CompSrcInstIx Int
  deriving (Eq, Show)

-- | Hand-rolled rather than derived: this newtype is defined without
-- 'GeneralizedNewtypeDeriving' in scope (see this module's own pragma
-- list), and a manual instance is one line either way. Needed so
-- 'rs_srcLocks' below can key a 'HashMap' on this type directly, the same
-- way 'rs_srcs' keys one on 'CompSrcId'.
instance Hashable CompSrcInstIx where
  hashWithSalt s (CompSrcInstIx n) = hashWithSalt s n

firstCompSrcInstIx :: CompSrcInstIx
firstCompSrcInstIx = CompSrcInstIx 0

nextCompSrcInstIx :: CompSrcInstIx -> CompSrcInstIx
nextCompSrcInstIx (CompSrcInstIx n) = CompSrcInstIx (n + 1)

data RegState = RegState
  { rs_srcs :: HashMap CompSrcId (CompSrcInstIx, AnyCompSrc)
  , rs_sinks :: HashMap CompSinkId AnyCompSink
  , rs_nextSrcIx :: CompSrcInstIx
  , rs_srcLocks :: HashMap CompSrcInstIx (MVar ())
  -- ^ One dedicated lock per registered source instance, created in
  -- 'registerCompSrc' and never removed (mirrors 'CompSrcInstIx' itself
  -- never being reused across an unregister\/re-register cycle -- see its
  -- haddock). Consulted only via 'lookupSrcLock', and only when parallel
  -- eval is enabled and the instance is 'Control.Computations.CompEngine.CompSrc.FlowSerial'
  -- -- see "Control.Computations.CompEngine.Impl"'s @getOrCreateGroup@,
  -- the only caller, and 'Control.Computations.CompEngine.CompSrc.compSrcConcurrency's
  -- own haddock for the guarantee this restores now that a
  -- 'Control.Computations.CompEngine.CompSrc.FlowSerial' source can be
  -- reached from two concurrently-forked eval-leaf threads at once.
  , rs_sinkLocks :: HashMap CompSinkId (MVar ())
  -- ^ The sink-side sibling of 'rs_srcLocks' -- see 'withSinkInstLock', the
  -- only place this is read. Keyed on 'CompSinkId' directly rather than a
  -- dense index the way 'rs_srcLocks' is keyed on 'CompSrcInstIx': there is
  -- no existing per-batch hot path hashing a sink id the way
  -- "Control.Computations.CompEngine.Impl"'s @CompReqCombined@ preparation
  -- does for sources (see 'CompSrcInstIx'\'s own haddock), so there is
  -- nothing here for a dense index to save.
  }

{- | Width of the engine's nested-eval fork pool (see
 "Control.Computations.CompEngine.Impl".'Control.Computations.CompEngine.Impl.ParState') --
 how many cap evaluations may run concurrently, forked off a
 'CompReqCombined' batch's eval leaves. 1 (the default -- see
 'newCompFlowRegistry') means no fork pool at all: every eval leaf runs
 inline, in leaf order, on the engine thread, exactly as before this knob
 existed.

 Deliberately just an 'Int' wrapper, not e.g. a number-of-capabilities
 lookup or anything more elaborate: how wide to run is a property of the
 workload (how much genuinely-concurrent work it has, how deep the fan-out
 goes), not of the machine, and is exactly as easy to get wrong in either
 direction, so this leaves the choice entirely to the caller.
-}
newtype CompEvalConcurrency = CompEvalConcurrency {unCompEvalConcurrency :: Int}
  deriving (Eq, Show)

{- | Clamp to at least 1. A width of 0 (or negative) would mean the fork
 pool never has any permits at all -- silently disabling forking rather
 than just leaving it at width 1 -- so this floors at the smallest
 meaningful width instead of letting a caller construct that footgun.
-}
mkCompEvalConcurrency :: Int -> CompEvalConcurrency
mkCompEvalConcurrency = CompEvalConcurrency . max 1

{- | The set of sources and sinks currently available to comp bodies, plus
 the 'CompEvalConcurrency' width the engine's nested-eval fork pool should be
 built at (see 'setCompEvalConcurrency'\/'readCompEvalConcurrency'). Create
 one with 'newCompFlowRegistry', populate it with 'registerCompSrc'\/
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
    'setCompEvalConcurrency' on that same registry, from that same callback,
    entirely without forking the driver or adding a parameter to it.
-}
data CompFlowRegistry = CompFlowRegistry
  { cfr_state :: TVar RegState
  , cfr_evalConcurrency :: TVar CompEvalConcurrency
  -- ^ see 'setCompEvalConcurrency'. Read exactly once, by
  -- "Control.Computations.CompEngine.Impl".'Control.Computations.CompEngine.Impl.initCompEngine',
  -- at engine start -- see 'readCompEvalConcurrency's haddock for why.
  }

newCompFlowRegistry :: IO CompFlowRegistry
newCompFlowRegistry = do
  let state = RegState HashMap.empty HashMap.empty firstCompSrcInstIx HashMap.empty HashMap.empty
  v <- newTVarIO state
  evalConcVar <- newTVarIO (mkCompEvalConcurrency 1)
  pure (CompFlowRegistry v evalConcVar)

{- | Set the width 'Control.Computations.CompEngine.Impl.initCompEngine' will
 use for @reg@'s nested-eval fork pool (see
 'Control.Computations.CompEngine.Impl.ParState').

 __Must be called before the engine that reads @reg@ starts__: this is read
 exactly once, by @initCompEngine@, at engine construction -- a call after
 that point has no effect on the already-running engine. That's a
 deliberate, documented difference from a knob that took effect
 mid-run, not an oversight: the fork pool (@ParState@'s permit
 'Control.Concurrent.STM.TVar' and promise registry) is sized and allocated
 once, at engine start, since a batch's eval leaves fork against one shared,
 engine-lifetime pool rather than a per-batch worker count.
-}
setCompEvalConcurrency :: CompFlowRegistry -> CompEvalConcurrency -> IO ()
setCompEvalConcurrency reg = atomically . writeTVar (cfr_evalConcurrency reg)

-- | Read back @reg@'s eval-concurrency width -- see 'setCompEvalConcurrency'.
readCompEvalConcurrency :: CompFlowRegistry -> IO CompEvalConcurrency
readCompEvalConcurrency = readTVarIO . cfr_evalConcurrency

withTypedCompSrcId
  :: forall m a s
   . (MonadIO m, CompSrc s)
  => CompFlowRegistry
  -> TypedCompSrcId s
  -> (s -> m a)
  -> m (Fail a)
withTypedCompSrcId reg typedKey fun =
  withTypedCompSrcIdIndexed reg typedKey (const fun)

{- | Like 'withTypedCompSrcId', but also hands @fun@ this instance's dense
 'CompSrcInstIx' (see its haddock) alongside the live instance, both read
 from the very same registry lookup -- for
 "Control.Computations.CompEngine.Impl"'s 'CompReqCombined' batch
 preparation only, which needs the index to key its per-batch source-group
 table without hashing a 'CompSrcId' a second time. Every other caller wants
 plain 'withTypedCompSrcId'.
-}
withTypedCompSrcIdIndexed
  :: forall m a s
   . (MonadIO m, CompSrc s)
  => CompFlowRegistry
  -> TypedCompSrcId s
  -> (CompSrcInstIx -> s -> m a)
  -> m (Fail a)
withTypedCompSrcIdIndexed reg typedKey fun =
  withCompSrcIdIndexed reg (unTypedCompSrcId typedKey) fun

withCompSrcIdIndexed
  :: forall m a s
   . (MonadIO m, CompSrc s)
  => CompFlowRegistry
  -> CompSrcId
  -> (CompSrcInstIx -> s -> m a)
  -> m (Fail a)
withCompSrcIdIndexed reg key fun = do
  regState <- liftIO $ readTVarIO (cfr_state reg)
  case Map.lookup key (rs_srcs regState) of
    Just (ix, AnyCompSrc src) ->
      case cast src of
        Just src' -> Ok <$> fun ix src'
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
    F.forM_ (rs_srcs regState) $ \(_, AnyCompSrc src) -> srcFun src

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
    -- The lock is allocated here, in IO, rather than inside the atomically
    -- block below: STM transactions may retry, and a retried 'newMVar ()'
    -- would allocate (and discard) a fresh lock every retry -- fine for
    -- correctness (only the MVar committed by the winning attempt is ever
    -- looked up again), but pointless churn next to just building it once,
    -- outside the transaction, the same way 'registerCompSink' below does.
    lock <- newMVar ()
    atomically $ modifyTVar' (cfr_state reg) $ \state ->
      state
        { rs_srcs = HashMap.insert (compSrcId src) (rs_nextSrcIx state, AnyCompSrc src) (rs_srcs state)
        , rs_nextSrcIx = nextCompSrcInstIx (rs_nextSrcIx state)
        , rs_srcLocks = HashMap.insert (rs_nextSrcIx state) lock (rs_srcLocks state)
        }

unregisterCompSrc :: CompSrc s => CompFlowRegistry -> s -> IO ()
unregisterCompSrc reg src =
  do
    logInfo ("Unregistering " ++ show (compSrcId src))
    -- NOTE: rs_srcLocks is deliberately left untouched here -- see its own
    -- haddock. The ix this src held is never reused (CompSrcInstIx's own
    -- invariant), so the orphaned lock entry can never be mistaken for a
    -- different, later instance's lock; it simply outlives the src that
    -- last used it, the same way the ix itself does.
    atomically $ modifyTVar' (cfr_state reg) $ \state ->
      state{rs_srcs = HashMap.delete (compSrcId src) (rs_srcs state)}

-- | Make a sink available to comp bodies via 'Control.Computations.CompEngine.Types.compSinkReq'.
registerCompSink :: CompSink s => CompFlowRegistry -> s -> IO ()
registerCompSink reg sink =
  do
    logInfo ("Registering " ++ show (compSinkId sink))
    lock <- newMVar ()
    atomically $ modifyTVar' (cfr_state reg) $ \state ->
      state
        { rs_sinks = HashMap.insert (compSinkId sink) (AnyCompSink sink) (rs_sinks state)
        , rs_sinkLocks = HashMap.insert (compSinkId sink) lock (rs_sinkLocks state)
        }

unregisterCompSink :: CompSink s => CompFlowRegistry -> s -> IO ()
unregisterCompSink reg sink =
  do
    logInfo ("Unregistering " ++ show (compSinkId sink))
    -- rs_sinkLocks is left untouched here for the same reason
    -- unregisterCompSrc leaves rs_srcLocks untouched -- see that function's
    -- NOTE.
    atomically $ modifyTVar' (cfr_state reg) $ \state ->
      state{rs_sinks = HashMap.delete (compSinkId sink) (rs_sinks state)}

{- | Look up @ix@'s dedicated lock, created once at 'registerCompSrc' time --
 every registered source instance has exactly one, for the lifetime of the
 registry (never removed by 'unregisterCompSrc' -- see its own NOTE). Only
 ever called from "Control.Computations.CompEngine.Impl"'s
 @getOrCreateGroup@, and only for an @ix@ it just resolved through this same
 registry (via 'withTypedCompSrcIdIndexed'), so the error branch here should
 be unreachable outside a broken invariant -- it exists purely so a future
 regression fails loudly instead of silently skipping the lock it should
 have taken.
-}
lookupSrcLock :: CompFlowRegistry -> CompSrcInstIx -> IO (MVar ())
lookupSrcLock reg ix = do
  regState <- readTVarIO (cfr_state reg)
  case HashMap.lookup ix (rs_srcLocks regState) of
    Just lock -> pure lock
    Nothing -> error ("lookupSrcLock: no lock registered for source index " ++ show ix)

{- | Run @act@ under @sid@'s dedicated sink lock, but only when @parEnabled@
 (the calling engine has parallel eval turned on --
 "Control.Computations.CompEngine.Impl"'s @ce_par@ is 'Just') /and/ @conc@
 is 'Control.Computations.CompEngine.CompSrc.FlowSerial'
 ('Control.Computations.CompEngine.CompSink.compSinkConcurrency's own
 default) -- see that method's haddock for the guarantee this restores.
 Otherwise -- eval width 1, or a sink that has declared
 'Control.Computations.CompEngine.CompSrc.FlowConcurrent' -- this just runs
 @act@ directly, touching neither the registry nor any lock.

 __Why this can't deadlock__: the only callers
 ("Control.Computations.CompEngine.Impl"'s @doCompSinkReq@\/
 @doCompSinkReqValue@) hold this lock across exactly one
 'Control.Computations.CompEngine.CompSink.compSinkExecute' call and
 nothing else -- no nested cap evaluation happens, and no promise-table
 join happens, while it's held. So a thread waiting on (or holding) this
 lock never appears in the promise table's wait-for graph -- it is either
 doing bounded I\/O against this one sink instance, or waiting for another
 thread that is. That is the same "claim on start, and only while doing
 real work" argument
 "Control.Computations.CompEngine.Impl"'s @claimOrJoin@ haddock makes for
 why a genuine deadlock there is always a real dependency cycle, applied to
 a lock instead of a promise: this lock can only ever be contended by two
 threads that both, right now, want to do I\/O against the same sink, never
 by a thread that is itself blocked waiting on the thread trying to acquire
 it.
-}
withSinkInstLock :: CompFlowRegistry -> Bool -> CompSinkId -> FlowConcurrency -> IO a -> IO a
withSinkInstLock reg parEnabled sid conc act
  | parEnabled && conc == FlowSerial = do
      regState <- readTVarIO (cfr_state reg)
      case HashMap.lookup sid (rs_sinkLocks regState) of
        Just lock -> withMVar lock (const act)
        Nothing -> error ("withSinkInstLock: no lock registered for sink " ++ show sid)
  | otherwise = act

data Blocking = Block | DontBlock
  deriving (Eq, Show)

allCompSrcChanges :: CompFlowRegistry -> Blocking -> STM (HashSet AnyCompSrcDep)
allCompSrcChanges reg b =
  do
    regState <- readTVar (cfr_state reg)
    let srcs = rs_srcs regState
    logTraceSTM ("Waiting for changes on " ++ show (Map.size srcs) ++ " sources")
    -- NOTE: toList (not elems), so each instance's own registry key --
    -- already computed once at 'registerCompSrc' time -- comes along for
    -- 'getChanges' to reuse via 'wrapCompSrcDepWithId', instead of every
    -- changed dep re-deriving it from the instance via 'wrapCompSrcDep' (see
    -- that function's haddock).
    let changesActions :: [STM (HashSet AnyCompSrcDep)]
        changesActions = map (\(cid, (_, AnyCompSrc src)) -> getChanges cid src) (HashMap.toList srcs)
    -- wait for at least one change
    set1 <-
      case b of
        Block -> F.foldl' orElse retry changesActions
        DontBlock -> pure HashSet.empty
    -- collect all changes
    sets <- mapM (`orElse` pure HashSet.empty) changesActions
    pure (HashSet.unions (set1 : sets))
 where
  getChanges :: forall s. CompSrc s => CompSrcId -> s -> STM (HashSet AnyCompSrcDep)
  getChanges cid s =
    do
      deps <- compSrcWaitChanges s
      when (HashSet.null deps) $
        logDebugSTM (show cid ++ " returned no results, it should have blocked in STM.")
      pure (HashSet.map (wrapCompSrcDepWithId (Proxy @s) cid) deps)
