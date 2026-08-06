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
  peekPromiseAwaits,
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
import Control.Computations.Utils.ConcUtils (cancelAllTracked, dispatchJobs, forkTracked, trySync)
import Control.Computations.Utils.Logging
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import Control.Concurrent (rtsSupportsBoundThreads)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (ErrorCall (..), SomeException, throwIO)
import qualified Control.Exception
import Control.Monad
import Control.Monad.Reader
-- Strict, not the mtl default (Control.Monad.State re-exports the Lazy
-- variant) -- see CompEngineM's Monad instance below for why this matters
-- on the suspend/resume hot path.
import Control.Monad.State.Strict
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Typeable (Proxy (..), cast)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import Text.Printf (printf)

data CompEngine = CompEngine
  { ce_compEngineIfs :: CompEngineIfs
  , ce_capSeqCounter :: IORef Word64
  -- ^ see 'CapSeq'\'s haddock -- one monotone counter, shared by every
  -- 'tellGarbage' call for the lifetime of this engine, so stamps compare
  -- correctly across every accumulation window, not just within one.
  , ce_par :: Maybe ParState
  -- ^ 'Nothing' unless 'CompFlowRegistry.readCompEvalConcurrency' reported a
  -- width above 1 at 'initCompEngine' time -- see 'ParState'\'s haddock.
  -- __Every code path touching this is a @case@ on this pointer, and the
  -- 'Nothing' branch is always exactly the code that existed before this
  -- field did.__ That is not a style preference: this project's own
  -- acceptance bar is that a default (width-1) engine produces bit-identical
  -- @allocated_bytes@\/@max_live_bytes@ to the unmodified engine, and the
  -- only way to promise that honestly is for the width-1 path to literally
  -- be the old code, not a parameterised generalisation of it.
  , ce_chain :: EvalChain
  -- ^ this __logical thread of control__\'s in-progress ancestors -- see
  -- 'EvalChain'. Only ever meaningfully populated when 'ce_par' is 'Just'
  -- (see 'doCompAp'); at 'Nothing' nothing ever reads or extends it.
  , ce_diag :: Maybe EngineDiag
  -- ^ 'Nothing' unless @COMP_ENGINE_LOCK_STATS@ was set at 'initCompEngine'
  -- time -- see 'EngineDiag'\'s haddock. Deliberately independent of
  -- 'ce_par': the eval-leaf-per-batch histogram (see 'countEvalLeaves') is a
  -- structural property of the graph, not of forking, and is exactly as
  -- worth measuring at width 1 (no permit pool at all) as at any other
  -- width -- so this field is set purely from the env var, never from
  -- whether 'ce_par' happens to be 'Just'.
  }

{- | A permit pool plus a promise registry for nested cap evaluation, built
 once by 'initCompEngine' when
 'Control.Computations.CompEngine.CompFlowRegistry.readCompEvalConcurrency'
 reports a width above 1, and shared (by reference, the same value) across
 every thread the engine ever forks -- see 'CompEngine'\'s 'ce_par'.

 * 'ps_promises' is filled in by 'claimOrJoin' -- see 'EvalPromise'\/
   'SomeEvalPromise' and 'doAnyEvalReqValue'\'s @Just@ branch.
 * 'ps_permits' is a counting permit pool of size @width - 1@ (the batch's
   own calling thread always keeps at least one leaf's work for itself --
   see 'prepEvalLeaf'), decremented by a successful 'tryAcquirePermit' and
   incremented back by 'releasePermit' as soon as the forked leaf's own
   work finishes, whatever the outcome -- __not__ when some later caller
   gets around to 'Async.wait'-ing it (see 'prepEvalLeaf'\'s own note on why
   tying release to join order, rather than to actual completion, throttles
   the whole pool behind whichever leaf is slowest to be joined).
 * 'ps_width' is the raw width, kept alongside the derived permit count
   purely so a diagnostic doesn't have to reconstruct it from
   @width - 1@ backwards.
-}
data ParState = ParState
  { ps_promises :: !(IORef (HashMap AnyCompAp SomeEvalPromise))
  , ps_permits :: !(TVar Int)
  , ps_width :: !Int
  }

{- | This logical thread of control's in-progress ancestors, plus a slot for
 what it is currently blocked waiting on.

 'ec_ancestors' is extended by 'doCompAp' every time a cap actually begins
 evaluating (not on a cache hit -- see 'doCompAp'\'s @Just@ branch), and
 checked *before* extending: a cap already present is a direct cycle,
 reported immediately (see 'directCycleMsg') rather than left to recurse to
 stack exhaustion, which is what happens today when 'ce_par' is 'Nothing'
 (a cold self-dependency is indistinguishable from "never evaluated" to
 'Control.Computations.CompEngine.Core.lookupCapResult', which maps both to
 'Control.Computations.CompEngine.Core.CapNotFound'). 'CompAp'\'s own
 identity ('AnyCompAp'\'s 'Eq', keyed on the interned param hash) is exactly
 the identity 'ec_ancestors' compares on, so this catches a cycle on the
 *value*, the same granularity the cache and the promise table both key on.

 A __fresh__ 'EvalChain' is built for every forked eval leaf (see
 'prepEvalLeaf'): its 'ec_ancestors' seeds from the forking thread's own (the
 fork is still, logically, a descendant of everything its parent was already
 evaluating), but it gets its own fresh 'ec_awaiting' -- a real OS/green
 thread can only ever be blocked on one thing at a time, so that slot is
 per-thread, not per-nesting-level, and stays the same 'IORef' across every
 'doCompAp'-driven descent within one thread (only 'ec_ancestors' changes as
 the chain grows).

 'ec_awaiting' is written (and cleared in a 'Control.Exception.finally') by
 a *joiner* immediately around the blocking 'readMVar' in
 'doAnyEvalReqValue'\'s @Just@ branch. It is not consulted by any check
 today -- see that function's haddock for the argument that makes a live
 consultation unnecessary (the promise table's "claim on start" discipline
 is what keeps every genuine deadlock a real dependency cycle, catchable by
 the runtime's own indefinite-block detector) -- it exists so a future
 diagnostic walking every live 'EvalChain' (e.g. on a
 'BlockedIndefinitelyOnMVar') can answer "what was this thread doing"
 without new bookkeeping.
-}
data EvalChain = EvalChain
  { ec_ancestors :: !(HashSet AnyCompAp)
  , ec_awaiting :: !(IORef (Maybe AnyCompAp))
  }

{- | A write-once result cell for one cap's in-flight evaluation, claimed by
 exactly one caller (the "owner") and read by any number of others (the
 "joiners") -- see 'doAnyEvalReqValue'\'s @Just@ branch, the only code that
 creates, fills, or reads one.

 'ep_cell' holds 'Left' on a synchronous failure (so a joiner rethrows
 exactly what the owner's evaluation itself threw, the same attribution
 'Async.wait' would give a genuine fork) or 'Right' on success, and is
 filled with 'putMVar' exactly once, by the owner, always -- even on the
 failure path -- so no joiner can block on it forever. 'ep_chain' records
 the owner's 'EvalChain' *at the moment it claimed this cap* (before
 'doCompAp' extends it further); nothing reads it today (see 'EvalChain'\'s
 haddock on 'ec_awaiting' for why an active cross-thread check is
 unnecessary here), it is carried for the same future-diagnostic reason.
-}
data EvalPromise a = EvalPromise
  { ep_cell :: !(MVar (Either SomeException (Maybe (CompApResult a))))
  , ep_chain :: !EvalChain
  }

-- | 'EvalPromise' with its result type hidden -- what 'ParState'\'s
-- 'ps_promises' registry actually stores one of per in-flight 'AnyCompAp',
-- since different claimants' result types are only known once resolved
-- back through the 'cast' in 'claimOrJoin'.
data SomeEvalPromise = forall a. IsCompResult a => SomeEvalPromise (EvalPromise a)

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
  , sg_lock :: Option (MVar ())
  -- ^ 'Some' iff this group's instance actually needs 'runGroupOnce' to
  -- serialize against a concurrently-forked sibling -- i.e. iff parallel
  -- eval is enabled *and* @sg_conc@ is 'FlowSerial' -- resolved once, here,
  -- at group-creation time (see 'getOrCreateGroup'), rather than
  -- re-checked on every 'runGroupOnce' call: the group is only ever
  -- triggered once anyway (see 'sg_triggered'), so there is nothing to
  -- gain from deferring the lookup, and doing it once keeps 'runGroupOnce'
  -- itself a plain 'SrcGroup' consumer with no registry\/'ce_par' access of
  -- its own to thread through.
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
  => CompFlowRegistry
  -> Bool
  -- ^ whether this engine has parallel eval enabled (@ce_par@ is 'Just') --
  -- see 'sg_lock'.
  -> IORef [(CompSrcInstIx, SomeSrcGroup)]
  -> CompSrcInstIx
  -> s
  -> FlowConcurrency
  -> IO (SrcGroup s)
getOrCreateGroup reg parEnabled groupsRef ix src conc = do
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
      lock <-
        if parEnabled && conc == FlowSerial
          then Some <$> lookupSrcLock reg ix
          else pure None
      let g = SrcGroup src conc fetchesRef triggeredRef lock
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

 __Locking and why it can't deadlock__: when 'sg_lock' is 'Some' (parallel
 eval enabled, this instance 'FlowSerial' -- see 'getOrCreateGroup'), the
 whole 'compSrcExecuteBatch' call below runs under that lock, restoring
 'Control.Computations.CompEngine.CompSrc.compSrcConcurrency's documented
 guarantee ("the engine runs this source's requests one at a time") for a
 'FlowSerial' instance even when two different eval leaves, forked onto two
 different threads by "Control.Computations.CompEngine.Impl"'s
 @prepEvalLeaf@, each build their own nested batch against it -- exactly
 the scenario 'FlowSerial'\'s own haddock now has to account for. This
 can't deadlock: the lock is held only across this one call, during which
 the holding thread evaluates no cap and joins no promise (@fetches@'
 callbacks just 'writeIORef' a slot -- see 'prepSrcLeaf'), so it never
 appears in the promise table's wait-for graph. A thread blocked here is
 always blocked on another thread doing real, bounded I\/O against the same
 source instance, never on a thread that is itself waiting on this one --
 the same "claim on start, only while doing real work" argument
 'claimOrJoin'\'s haddock makes for why a genuine deadlock there is always a
 real dependency cycle.
-}
runGroupOnce :: forall s. CompSrc s => SrcGroup s -> IO ()
runGroupOnce g = do
  alreadyRan <- atomicModifyIORef' (sg_triggered g) (\ran -> (True, ran))
  unless alreadyRan $ do
    fetches <- ($ []) <$> readIORef (sg_fetches g)
    result <- trySync (withGroupLock (sg_lock g) (compSrcExecuteBatch (sg_src g) fetches))
    case result of
      Right () -> pure ()
      Left ex -> forM_ fetches $ \(SrcFetch _ write) -> write (Left ex)
 where
  withGroupLock :: Option (MVar ()) -> IO a -> IO a
  withGroupLock None act = act
  withGroupLock (Some lock) act = withMVar lock (const act)

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

-- | Merge a forked eval leaf's own 'GenDel' (accumulated on its own thread,
-- against its own fresh 'runCompEngineM'' state -- see 'prepEvalLeaf') into
-- the calling thread's, once the fork has been joined. Exactly the
-- commutative union every other 'GenDel' merge in this module already uses
-- ('Semigroup GenDel' above) -- see 'CapSeq'\'s haddock for why that union
-- was made merge-safe specifically to support this.
mergeGenDel :: GenDel -> CompEngineM ()
mergeGenDel gd = CompEngineM (modify' (<> gd))
{-# INLINE mergeGenDel #-}

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

----------------------------------------------------------------------------
-- Parallel-evaluation plumbing: EvalChain scoping, exception-safe wrappers,
-- and the promise-table claim/join dance. Every one of these is gated on
-- 'ce_par' by its only callers ('doCompAp', 'doAnyEvalReqValue') -- nothing
-- here runs at all when 'ce_par' is 'Nothing'.
----------------------------------------------------------------------------

{- | Run @act@ against @f@'s modification of the current 'CompEngine' (most
 callers: swap in an extended 'ce_chain'), restoring the ambient environment
 once @act@ returns -- 'CompEngineM'\'s own analogue of
 'Control.Monad.Reader.local', built on 'runCompEngineM'/'CompEngine' rather
 than a real 'Control.Monad.Reader.MonadReader' instance (this newtype
 deliberately has none -- see its haddock) so nesting composes correctly:
 @act@ runs with @f@ applied, and whatever it threads back through 'GenDel'
 is exactly what the caller's own state becomes next, same as an ordinary
 monadic bind.
-}
localCompEngine :: (CompEngine -> CompEngine) -> CompEngineM a -> CompEngineM a
localCompEngine f act = CompEngineM $ do
  s0 <- get
  ce <- asks f
  (a, s') <- lift (lift (runCompEngineM act ce s0))
  put s'
  pure a

{- | Run @act@, then @cleanup@ unconditionally (success, synchronous failure,
 or asynchronous cancellation alike) -- the 'CompEngineM' analogue of
 'Control.Exception.finally', for the same reason 'localCompEngine' exists:
 there is no 'Control.Monad.Catch.MonadMask' instance for this newtype, and
 building one isn't worth it for the two call sites ('prepEvalLeaf' cancelling
 the batch's still-pending forks and 'doAnyEvalReqValue'\'s joiner clearing
 'ec_awaiting') that need this shape.
-}
finallyCompEngineM :: CompEngineM a -> IO () -> CompEngineM a
finallyCompEngineM act cleanup = CompEngineM $ do
  s0 <- get
  ce <- asks id
  (a, s') <- lift (lift (runCompEngineM act ce s0 `Control.Exception.finally` cleanup))
  put s'
  pure a

{- | Run @act@, turning a __synchronous__ exception into a 'Left' instead of
 letting it propagate -- the 'CompEngineM' analogue of 'Control.Exception.try'
 (unlike "Control.Computations.Utils.ConcUtils".'trySync', this does not
 special-case asynchronous exceptions: see 'doAnyEvalReqValue'\'s owner
 branch, the only caller, for why a plain 'Control.Exception.try' is
 sufficient there). On failure the enclosing 'GenDel' is left exactly as it
 was *before* @act@ ran -- a caught exception means @act@\'s own state
 changes never really committed (the same "no partial state survives an
 exception" rule an uncaught exception already enforces at the very top of
 the engine, e.g. 'stepCompEngine'\'s caller), not that they're replaced with
 'mempty'.
-}
tryCompEngineM :: CompEngineM a -> CompEngineM (Either SomeException a)
tryCompEngineM act = CompEngineM $ do
  s0 <- get
  ce <- asks id
  outcome <- lift (lift (Control.Exception.try (runCompEngineM act ce s0)))
  case outcome of
    Left e -> pure (Left e)
    Right (a, s') -> put s' >> pure (Right a)

-- | The error 'doCompAp' raises when @key@ is already one of @chain@\'s own
-- ancestors -- a direct, same-logical-thread cycle. Unordered (a 'HashSet'
-- has no call order to render), but still names every cap on the chain,
-- which is enough to diagnose the cycle by hand.
directCycleMsg :: AnyCompAp -> EvalChain -> String
directCycleMsg key chain =
  "doCompAp: direct cycle detected evaluating "
    ++ show key
    ++ " -- already mid-evaluation on this thread's chain: "
    ++ show (HashSet.toList (ec_ancestors chain))

----------------------------------------------------------------------------
-- Fork\/occupancy\/shape diagnostics -- gated on @COMP_ENGINE_LOCK_STATS@
-- (see 'CompEngine'\'s 'ce_diag' haddock for why this reuses that flag
-- rather than adding a second one, and
-- "Control.Computations.CompEngine.Run"'s @setupSimpleStateIf@ for the
-- state-if half of the same flag). Answers a different question than that
-- module's lock-wait\/hold instrumentation does -- "where does the
-- fork-eligible parallelism actually go", not "how much does the state-if
-- lock cost" -- but is reported at the same teardown moment (see
-- 'reportEngineDiag' and 'stopCompEngine') and gated by the same flag, so
-- there is exactly one on/off switch for "am I paying to measure the
-- engine".
----------------------------------------------------------------------------

{- | Time-weighted permit occupancy, folded by 'bumpOccupancy' on every
 acquire and release. 'os_lastTs'\/'os_integralNs' together let a
 teardown-time read compute a genuine time-weighted __mean__ (the integral
 of permits-in-use over elapsed time, divided by elapsed time) rather than
 a peak -- the per-source concurrency high-water marks
 ("Demos.Bench.SystemSrc") already show the peak, and this project's own
 measurement notes found a high-water mark of 16 misleading on its own
 (reached briefly, not sustained) -- see @docs\/benchmark-notes.md@'s
 tiered-plateau stage.
-}
data OccupancyState = OccupancyState
  { os_lastTs :: !Word64
  , os_inUse :: !Int
  , os_integralNs :: !Word64
  }

{- | One instance per engine, built by 'initCompEngine' only when
 @COMP_ENGINE_LOCK_STATS@ is set (see 'ce_diag').

 * 'ed_forkAttempts'\/'ed_forkSuccesses'\/'ed_forkFailures': every call
   'PendingEvalLeaf'\'s @pel_tryStart@ makes to 'tryAcquirePermit' bumps
   @attempts@, then exactly one of @successes@\/@failures@. Since
   'topUpWindow' can retry the same still-queued leaf from many different
   later leaves' own join points (see its haddock), one logical leaf can
   now contribute more than one @attempts@\/@failures@ pair before it
   either succeeds or falls back to running inline -- unlike the
   collect-time-only decision this replaced, where each eligible leaf
   contributed exactly one. @failures@ that never resolve into a success
   are 'prepEvalLeaf's Cilk-style serial elision (the leaf ran inline
   instead of forking), not an error -- see that function's own haddock.
 * 'ed_occupancy': see 'OccupancyState'.
 * 'ed_depthHist': keyed by 'ec_ancestors'\'s size (the *forking* thread's
   own chain depth -- the forked child sits one level deeper) at every
   *successful* fork. A starved attempt never reaches a depth worth
   recording, since it ran inline instead.
 * 'ed_leafHist': keyed by a batch's total 'CompReqEval' leaf count (see
   'countEvalLeaves'), one entry per 'CompReqCombined' batch reaching
   'doSuspended' -- independent of 'ce_par' entirely, since this is a
   structural property of the graph, not of forking, and is exactly as
   interesting at width 1 (see 'CompEngine'\'s 'ce_diag' haddock).
 * 'ed_promiseAwaits': bumped once per call to 'doAnyEvalReqValue' that
   resolves as a 'ClaimJoiner' rather than a 'ClaimOwner' -- i.e. once per
   reference to a cap that found another reference's evaluation of the
   *same* cap already in flight and joined it instead of re-evaluating.
   Independently useful as a live dedup count (how much repeated-reference
   work the promise table is actually saving), and it is what
   "Control.Computations.CompEngine.Tests.TestCompEvalConcurrency"'s
   @test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8@
   polls (via 'peekPromiseAwaits') to know, deterministically rather than by
   timing, that a second reference has actually claimed a join before
   letting the first reference's (blocked, for exactly this test) evaluation
   proceed.

 'ed_windowStart' fixes the mean-occupancy denominator's start; see
 'reportEngineDiag'.
-}
data EngineDiag = EngineDiag
  { ed_windowStart :: !Word64
  , ed_forkAttempts :: !(IORef Word64)
  , ed_forkSuccesses :: !(IORef Word64)
  , ed_forkFailures :: !(IORef Word64)
  , ed_occupancy :: !(TVar OccupancyState)
  , ed_depthHist :: !(IORef (Map Int Word64))
  , ed_promiseAwaits :: !(IORef Word64)
  , ed_leafHist :: !(IORef (Map Int Word64))
  }

newEngineDiag :: IO EngineDiag
newEngineDiag = do
  t0 <- getMonotonicTimeNSec
  EngineDiag t0
    <$> newIORef 0
    <*> newIORef 0
    <*> newIORef 0
    <*> newTVarIO (OccupancyState t0 0 0)
    <*> newIORef Map.empty
    <*> newIORef 0
    <*> newIORef Map.empty

{- | Parses @COMP_ENGINE_LOCK_STATS@ exactly as
 "Control.Computations.CompEngine.Run"'s @setupSimpleStateIf@ does --
 duplicated, not imported, because this module sits *below* @Run.hs@ in the
 dependency graph (@Run.hs@ imports @Impl.hs@, not the reverse), the same
 reason "Demos.Bench.Hospital"\/"Demos.Bench.Tiered"\/"Demos.Bench.Main"
 each carry their own copy of their counting driver rather than importing
 one another's.
-}
lockStatsEnabled :: IO Bool
lockStatsEnabled = do
  mEnv <- lookupEnv "COMP_ENGINE_LOCK_STATS"
  pure $ case mEnv of
    Nothing -> False
    Just "" -> False
    Just "0" -> False
    Just _ -> True

-- | Run @act@ against this engine's 'EngineDiag' iff diagnostics are on --
-- a no-op, and @act@ never even forced, when 'ce_diag' is 'Nothing' (see
-- 'CompEngine'\'s own haddock on why that field exists independently of
-- 'ce_par').
whenDiag :: CompEngine -> (EngineDiag -> IO ()) -> IO ()
whenDiag ce act = case ce_diag ce of
  Nothing -> pure ()
  Just diag -> act diag
{-# INLINE whenDiag #-}

recordForkAttempt :: EngineDiag -> IO ()
recordForkAttempt diag = modifyIORef' (ed_forkAttempts diag) (+ 1)
{-# INLINE recordForkAttempt #-}

recordForkOutcome :: EngineDiag -> Bool -> IO ()
recordForkOutcome diag True = modifyIORef' (ed_forkSuccesses diag) (+ 1)
recordForkOutcome diag False = modifyIORef' (ed_forkFailures diag) (+ 1)
{-# INLINE recordForkOutcome #-}

{- | Fold one occupancy transition (@+1@ on a successful acquire, @-1@ on
 the matching release, once the forked leaf's own work actually finishes --
 see 'prepEvalLeaf') into 'ed_occupancy's running integral. The
 timestamp is read *outside* the 'atomically' block (an 'IO' action cannot
 run inside one); the transaction itself only does arithmetic on
 already-read values, so it stays a cheap, always-committing STM step.

 NOTE: @elapsed@ is clamped at 0 rather than left to underflow. Two
 concurrent callers each capture their own @now@ *before* entering
 'atomically'; if caller A's transaction retries after caller B's commits
 (using a later timestamp as the new @os_lastTs@), A's own, earlier @now@
 could be smaller than the @lastTs@ it reads on retry. This can only
 undercount a race's own sliver of elapsed time (nanoseconds, at STM
 retry rates), never blow up into the huge 'Word64' wraparound an
 unclamped subtraction would produce -- an acceptable, documented
 imprecision for a diagnostic, not worth a heavier-weight fix.
-}
bumpOccupancy :: EngineDiag -> Int -> IO ()
bumpOccupancy diag delta = do
  now <- getMonotonicTimeNSec
  atomically $ do
    st <- readTVar (ed_occupancy diag)
    let lastTs = os_lastTs st
        inUse = os_inUse st
        elapsed = if now >= lastTs then now - lastTs else 0
        integralNs' = os_integralNs st + fromIntegral inUse * elapsed
    writeTVar (ed_occupancy diag) (OccupancyState now (inUse + delta) integralNs')

recordDepth :: EngineDiag -> Int -> IO ()
recordDepth diag depth =
  atomicModifyIORef' (ed_depthHist diag) (\m -> (Map.insertWith (+) depth 1 m, ()))

-- | Bump 'ed_promiseAwaits' -- see that field's own haddock. Called from
-- 'doAnyEvalReqValue'\'s 'ClaimJoiner' branch, once per reference that
-- joins rather than owns.
recordPromiseAwait :: EngineDiag -> IO ()
recordPromiseAwait diag = modifyIORef' (ed_promiseAwaits diag) (+ 1)
{-# INLINE recordPromiseAwait #-}

recordLeafCount :: EngineDiag -> Int -> IO ()
recordLeafCount diag n =
  atomicModifyIORef' (ed_leafHist diag) (\m -> (Map.insertWith (+) n 1 m, ()))

-- | Count how many 'CompReqEval' leaves a 'CompReq' tree contains -- a pure
-- structural walk (no 'IO', no 'Prep') needed only for 'ed_leafHist', so it
-- costs nothing when diagnostics are off (never called -- 'whenDiag' never
-- forces its argument in the 'Nothing' case) and nothing beyond the tree's
-- own size when they are on.
countEvalLeaves :: forall r. CompReq r -> Int
countEvalLeaves (CompReqCombined x y) = countEvalLeaves x + countEvalLeaves y
countEvalLeaves (CompReqEval _) = 1
countEvalLeaves _ = 0

{- | The current value of 'ed_promiseAwaits', live -- unlike every other
 'EngineDiag' table, which is only ever read back at teardown (see
 'reportEngineDiag'). @0@, not an error, when @COMP_ENGINE_LOCK_STATS@
 diagnostics are off ('ce_diag' is 'Nothing'): a caller polling this to
 detect "a joiner has actually shown up" (see
 "Control.Computations.CompEngine.Tests.TestCompEvalConcurrency") is
 expected to have turned diagnostics on for exactly that reason, but a
 silent @0@ is a more honest answer than a partial function for the case
 where it didn't.

 Exported (unlike the rest of 'EngineDiag') purely so that test can observe
 a real engine-internal signal -- "the second reference to a cap has
 claimed a join on the first's still-in-flight evaluation" -- instead of
 inferring it from timing.
-}
peekPromiseAwaits :: CompEngine -> IO Word64
peekPromiseAwaits ce = maybe (pure 0) (readIORef . ed_promiseAwaits) (ce_diag ce)

{- | Print every 'EngineDiag' table, called once at teardown from
 'stopCompEngine' -- see that function's haddock for the 'finally' that
 guarantees this runs even when the engine is torn down by
 'Async.cancel' rather than by exhausting its run loop, the same
 guarantee @docs\/benchmark-notes.md@ relies on for @reportLockStats@. A
 no-op when @COMP_ENGINE_LOCK_STATS@ was off ('ce_diag' is 'Nothing').
-}
reportEngineDiag :: CompEngine -> IO ()
reportEngineDiag ce = case ce_diag ce of
  Nothing -> pure ()
  Just diag -> do
    attempts <- readIORef (ed_forkAttempts diag)
    successes <- readIORef (ed_forkSuccesses diag)
    failures <- readIORef (ed_forkFailures diag)
    now <- getMonotonicTimeNSec
    occ <- readTVarIO (ed_occupancy diag)
    let lastTs = os_lastTs occ
        finalIntegral =
          os_integralNs occ + fromIntegral (os_inUse occ) * (if now >= lastTs then now - lastTs else 0)
        elapsedNs = if now >= ed_windowStart diag then now - ed_windowStart diag else 0
        meanOccupancy
          | elapsedNs == 0 = 0
          | otherwise = fromIntegral finalIntegral / fromIntegral elapsedNs :: Double
        pct part whole
          | whole == (0 :: Word64) = 0
          | otherwise = 100 * fromIntegral part / fromIntegral whole :: Double
    depthHist <- readIORef (ed_depthHist diag)
    leafHist <- readIORef (ed_leafHist diag)
    promiseAwaits <- readIORef (ed_promiseAwaits diag)
    putStrLn "=== COMP_ENGINE_LOCK_STATS: eval-fork diagnostics ==="
    printf
      "fork attempts: %d, successes: %d (%.1f%%), permit-starved failures: %d (%.1f%%)\n"
      attempts
      successes
      (pct successes attempts)
      failures
      (pct failures attempts)
    printf
      "time-weighted mean permits in use: %.3f (window %.3f s, ending at teardown)\n"
      meanOccupancy
      (fromIntegral elapsedNs / (1e9 :: Double))
    printf "promise-table joins (repeated cap reference found the first still in flight): %d\n" promiseAwaits
    putStrLn "fork-depth histogram (forking thread's ancestor-chain depth -> successful forks):"
    forM_ (Map.toAscList depthHist) $ \(depth, n) -> printf "  depth %3d: %d\n" depth n
    putStrLn "eval-leaf-count-per-batch histogram (eval leaves in a CompReqCombined batch -> batch count):"
    forM_ (Map.toAscList leafHist) $ \(n, batches) -> printf "  %4d eval leaves: %d batches\n" n batches

-- | 'True' iff @0 < n@ and a permit was taken (@n - 1@ committed) --
-- 'STM'\'s own compare-and-swap, no blocking: a caller that loses the race
-- always falls back to running its work inline (see 'prepEvalLeaf'), never
-- retries or waits, which is what keeps a permit out of the wait-for graph
-- entirely (Cilk's work-first principle -- see 'prepEvalLeaf'\'s haddock).
tryAcquirePermit :: TVar Int -> IO Bool
tryAcquirePermit permits = atomically $ do
  n <- readTVar permits
  if n > 0
    then writeTVar permits (n - 1) >> pure True
    else pure False

-- | Give a permit taken by 'tryAcquirePermit' back to the pool -- called
-- once per successful acquire, unconditionally (success, failure, or
-- cancellation of the forked leaf that acquired it), from 'prepEvalLeaf'.
releasePermit :: TVar Int -> IO ()
releasePermit permits = atomically (modifyTVar' permits (+ 1))

{- | The sliding fork window: one not-yet-started eval leaf of a
 'CompReqCombined' batch, queued by 'prepEvalLeaf' during the batch's
 collect phase and consumed, front first, by 'topUpWindow' during the run
 phase.

 __Why a queue, not a one-shot decision.__ The fork/inline choice used to be
 made once per leaf, during the fast sequential IO walk that collects a
 batch's leaves ('traverseCompReq'), long before the run phase reaches any
 of them -- see @docs\/benchmark-notes.md@'s Stage 13. For a batch far wider
 than the permit pool (the tiered benchmark's dominant shape: 383 eval
 leaves against 7-15 permits), the first @width - 1@-many eligible leaves
 in traversal order got the only permits that would ever exist for that
 batch's whole lifetime; the remaining several hundred ran inline, one
 after another, even though the earliest forks typically finished and freed
 their permits back long before the batch itself finished. This type (and
 'topUpWindow') replace that single decision with a queue plus a per-leaf
 retry point: a leaf that loses the race for a permit stays queued rather
 than being condemned, and gets another chance every time a later leaf's
 own join point re-tops-up the window -- see 'prepEvalLeaf' for where a
 leaf's own join action calls 'topUpWindow' immediately before consulting
 its own state.

 'pel_tryStart' is the only thing this type carries: a self-contained
 attempt (closing over its own leaf's 'CompAp', private state cell,
 diagnostics, and the shared 'ParState'\/@forksRef@) that 'topUpWindow' can
 invoke without knowing anything about which leaf it belongs to -- the same
 existential-erasure trick 'SomeSrcGroup' already uses for a batch's source
 groups, just erasing down to @IO Bool@ instead of a typed record, since
 nothing outside the leaf's own closure ever needs its result type.
-}
newtype PendingEvalLeaf = PendingEvalLeaf
  { pel_tryStart :: IO Bool
  -- ^ Attempts exactly one 'tryAcquirePermit'. 'True': a permit was free,
  -- this leaf is now forked (its own private state cell -- see
  -- 'prepEvalLeaf' -- holds the resulting 'Async.Async' handle from this
  -- point on). 'False': the pool was empty at this instant; nothing about
  -- this leaf's own state changes, so it stays at the front of the queue
  -- for a future call to retry.
  }

{- | Start as many of @windowRef@'s not-yet-started eval leaves as there are
 free permits for, strictly from the front -- i.e. in the same left-to-right
 traversal order 'prepEvalLeaf' queued them in, __never__ skipping ahead to
 try a later leaf out of order. Called once at the top of every
 fork-eligible eval leaf's own join action (see 'prepEvalLeaf'), which is
 what gives a leaf that lost its own collect-time permit race more chances
 later, spread across the batch's whole run phase, instead of being
 condemned to run inline the moment that one collect-time attempt failed.

 Stops at the first 'False' rather than continuing past it: the queue's
 front is always this batch's next not-yet-started leaf in traversal order
 (every leaf before it has already been removed -- either by an earlier
 success here, or by its own join action's inline fallback once
 'topUpWindow' fails to reach it -- see 'prepEvalLeaf'), so leaving a failed
 leaf at the front is what lets the *next* call (from a later leaf's own
 join point, once an earlier fork has freed a permit) retry the very same
 leaf rather than silently reordering starts around it. Only __starts__ can
 float ahead of where the run phase currently is; nothing here ever changes
 __join__ order, which stays exactly the left-to-right order 'Prep'\'s own
 'Applicative' already sequences every leaf's own join action in.

 No cap on how far a single call can run ahead -- a call that keeps
 succeeding keeps going, all the way to an empty queue if the pool has that
 many permits free at that instant. The window's size is therefore bounded
 by the shared, engine-global permit pool alone, not by any new knob.
-}
topUpWindow :: IORef [PendingEvalLeaf] -> IO ()
topUpWindow windowRef = do
  pending <- readIORef windowRef
  case pending of
    [] -> pure ()
    (pel : rest) -> do
      started <- pel_tryStart pel
      when started $ do
        writeIORef windowRef rest
        topUpWindow windowRef

-- | Drop the queue's front entry unconditionally -- called only from a leaf
-- whose own join action just found itself still un-started after its own
-- 'topUpWindow' call (see 'prepEvalLeaf'), at which point that leaf, by
-- 'topUpWindow'\'s own invariant, is always the front. Removing it here is
-- what stops a future 'topUpWindow' call from retrying a leaf that has
-- already fallen back to running inline.
popWindowFront :: IORef [PendingEvalLeaf] -> IO ()
popWindowFront ref = atomicModifyIORef' ref $ \case
  [] -> ([], ())
  (_ : rest) -> (rest, ())

{- | The outcome of 'claimOrJoin': either this call is the first to reach
 @cap@ (becomes its "owner", and gets the fresh, still-empty cell to fill)
 or someone else already claimed it (this call becomes a "joiner", and gets
 their promise to block on).
-}
data EvalClaim a
  = ClaimOwner !(MVar (Either SomeException (Maybe (CompApResult a))))
  | ClaimJoiner !(EvalPromise a)

{- | Claim @cap@ in @par@\'s promise table, or discover someone else already
 has -- one 'atomicModifyIORef'' insert-if-absent, matching this codebase's
 established idiom for a claim that must not race (compare
 "Control.Computations.CompEngine.Impl"'s own 'runGroupOnce', or
 "Utils/DefTable.hs"'s row claims). A fresh, empty 'MVar' is allocated
 speculatively on every call (unavoidable: the value to insert-if-absent has
 to exist before the atomic step decides whether it's needed) and simply
 discarded on the losing path -- one wasted, tiny, unfilled 'MVar' per race
 lost is a trivial cost next to what it buys: the whole claim is a single
 lock-free op, not a read-then-maybe-insert two-step that could let two
 callers both believe they won.

 __This is where "claim on start, never in the collect phase" actually
 happens__ -- see 'doAnyEvalReqValue'\'s haddock for why that timing is what
 keeps every wait-for edge a genuine dependency edge.
-}
claimOrJoin
  :: forall a
   . IsCompResult a
  => ParState
  -> CompAp a
  -> EvalChain
  -> IO (EvalClaim a)
claimOrJoin par cap chain = do
  cell <- newEmptyMVar
  let key = AnyCompAp cap
      mine = SomeEvalPromise (EvalPromise cell chain)
  claimed <-
    atomicModifyIORef' (ps_promises par) $ \m ->
      case HashMap.lookup key m of
        Just existing -> (m, Left existing)
        Nothing -> (HashMap.insert key mine m, Right cell)
  case claimed of
    Right ownCell -> pure (ClaimOwner ownCell)
    Left (SomeEvalPromise existing) ->
      -- Cannot fail: 'AnyCompAp'\'s 'Eq' already performs the 'eqT' check
      -- that a 'HashMap.lookup' hit implies (see "Types.hs"), and
      -- 'IsCompResult' supplies the 'Typeable' this 'cast' needs -- a key
      -- match *is* a type match. Handled with a loud 'error' rather than a
      -- partial pattern match only so a future change breaking that
      -- invariant fails with a pointed message instead of a bare
      -- non-exhaustive-patterns crash.
      case cast existing of
        Just typed -> pure (ClaimJoiner typed)
        Nothing ->
          error
            ( "claimOrJoin: type mismatch resolving promise for "
                ++ show key
                ++ " -- should be impossible, see this function's haddock"
            )

-- | Remove @key@\'s entry from @par@\'s promise table once its owner has
-- filled the cell -- the registry only ever needs to answer "is this cap
-- being evaluated *right now*", not "was it ever", so an entry that
-- outlived its evaluation would wrongly turn a later, unrelated
-- re-evaluation of the same cap into a joiner on a stale result. Called
-- from 'doAnyEvalReqValue'\'s owner branch only *after* 'putMVar' has
-- already published the result -- a joiner that resolved this promise from
-- the table before this call can still safely block on its own copy of the
-- (by-reference) 'MVar' regardless of when the registry entry disappears.
releaseClaim :: ParState -> AnyCompAp -> IO ()
releaseClaim par key = atomicModifyIORef' (ps_promises par) (\m -> (HashMap.delete key m, ()))

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

-- | Whether this engine has parallel eval enabled ('ce_par' is 'Just') --
-- the same condition 'getOrCreateGroup' consults for 'sg_lock', reused
-- here by 'doCompSinkReq'\/'doCompSinkReqValue' so a sink's lock is, like a
-- source's, never even looked at when nothing could possibly be forking.
parEnabledM :: CompEngineM Bool
parEnabledM = CompEngineM (asks (isJust . ce_par))
{-# INLINE parEnabledM #-}

{- | Actually run @gap@\'s body (cache lookup/state-if bookkeeping aside --
 those already happened, or are about to, in whichever caller reached this:
 'evalWithCacheValue'\'s cache-miss branch for a nested eval, or 'execAp'
 directly for a top-level dequeue). This is also the single place that
 extends 'ce_chain' -- both of those callers reach a real cap evaluation
 exclusively through here, so scoping the chain at this one entry point
 covers a top-level dequeue calling back into itself just as much as an
 ordinary nested cycle (see 'EvalChain'\'s haddock) -- there is no second
 site that needs its own copy of this check.

 Gated on 'ce_par' being 'Just', per 'CompEngine'\'s own haddock: at
 'Nothing' this is 'doCompApBody' with zero extra work, byte for byte what
 this function was before 'EvalChain' existed.
-}
doCompAp
  :: IsCompResult a
  => CompAp a
  -> CompEngineM (Maybe (CompApResult a))
doCompAp gap =
  CompEngineM (asks ce_par) >>= \case
    Nothing -> doCompApBody
    Just _ -> do
      chain <- CompEngineM (asks ce_chain)
      let key = AnyCompAp gap
      when (key `HashSet.member` ec_ancestors chain) $
        liftIO (throwIO (ErrorCall (directCycleMsg key chain)))
      let chain' = chain{ec_ancestors = HashSet.insert key (ec_ancestors chain)}
      localCompEngine (\ce -> ce{ce_chain = chain'}) doCompApBody
 where
  doCompApBody =
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
  -- @withCapLookup cap f pure dequeued (evalWithCapCached cap f pure staleOk
  -- dequeued) capLookup >>= cont@ is the same computation as
  -- @withCapLookup cap f cont dequeued (evalWithCapCached cap f cont staleOk
  -- dequeued) capLookup@ for any @cont@ -- every branch of both either calls
  -- @cont@ (resp. @pure >=> cont@, which is just @cont@) directly, or calls
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
  -- | Looks up @cap@ and, in the same state-if round trip, dequeues it from
  -- the stale queue when the branch reached below calls for that -- see
  -- 'Control.Computations.CompEngine.Core.lookupCapResultDequeueIfStale's
  -- haddock for the fused contract and exactly which branches dequeue.
  -- 'withCapLookup'\/'evalWithCapCached' below used to each make their own
  -- 'dequeueGivenCap' call once they knew which branch they were in; now
  -- that decision has already been made (by the same policy, replicated in
  -- the state layer -- see that haddock again), so both just consume the
  -- @dequeued@ flag this returns instead of asking the state-if a second
  -- time.
  evalWithCacheValue
    :: forall x
     . IsCompResult x
    => Bool
    -> CompAp x
    -> CompEngineM (Maybe (CompApResult x))
    -> CompEngineM (Maybe (CompApResult x))
  evalWithCacheValue staleOk cap f = do
    (capLookup, dequeued) <- withCompState (\sif -> lookupCapResultDequeueIfStale sif staleOk cap)
    withCapLookup cap f pure dequeued (evalWithCapCached cap f pure staleOk dequeued) capLookup

  evalWithCapCached
    :: forall a b
     . CompAp a
    -> CompEngineM (Maybe (CompApResult a))
    -> (Maybe (CompApResult a) -> CompEngineM b)
    -> Bool
    -> Bool
    -- ^ whether the state-if lookup (see 'evalWithCacheValue') already
    -- dequeued @cap@ from the stale queue -- replaces the second
    -- 'dequeueGivenCap' round trip this branch used to make itself.
    -- 'CapMetaCached' never consults it: that branch recomputes
    -- unconditionally either way, and the fused lookup's policy guarantees
    -- it is 'False' there regardless.
    -> CapResult (CapCached a)
    -> CompEngineM b
  evalWithCapCached cap f cont staleOk dequeued capCached =
    case capCached of
      CapSuccess (CapValueCached a)
        | staleOk -> cont (Just a)
        | dequeued ->
            do
              logInfo ("Recalculating stale cap now " ++ capName)
              f >>= cont
        | otherwise ->
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
        | dequeued -> do
            logInfo ("Recalculating previously failing, stale cap now " ++ capName)
            f >>= cont
        | otherwise ->
            do
              logDebug ("Found valid cached failure for " ++ capName ++ ".")
              cont Nothing
   where
    capName = show cap

  withCapLookup
    :: forall a b c
     . CompAp a
    -> CompEngineM (Maybe (CompApResult a))
    -> (Maybe (CompApResult a) -> CompEngineM c)
    -> Bool
    -- ^ whether the state-if lookup (see 'evalWithCacheValue') already
    -- dequeued @cap@ -- always 'True' here in practice (a 'CapNotFound'
    -- lookup dequeues unconditionally, see that field's haddock), kept as a
    -- parameter rather than hardcoded so this function still says only what
    -- it's told, the same as before this fusion.
    -> (b -> CompEngineM c)
    -> CapLookup b
    -> CompEngineM c
  withCapLookup cap f cont dequeued withFound capLookup =
    case capLookup of
      CapFound found -> withFound found
      CapNotFound ->
        --  comp was never evaluated or was removed from cache
        -- NOTE: `f` runs here regardless of `dequeued` -- it only picks
        -- which string the log line below uses. This is therefore *not* a
        -- mutual-exclusion point: nothing here stops two callers from both
        -- reaching `f` for the same cap. What actually prevents duplicate
        -- evaluation is that cap evaluation stays on one (the engine)
        -- thread; `f`'s result gets recorded in the cache before any other
        -- cap evaluation on that thread can observe it.
        do
          logDebug
            ( (if dequeued then "Stale " else "Needed ")
                ++ show (capId cap)
                ++ " not cached.  Evaluating!"
            )
          f >>= cont

  {- | __Locking and why it can't deadlock__: @sinkFun@'s single
   'compSinkExecute' call below runs under @sinkId@'s dedicated lock
   whenever 'withSinkInstLock' decides one is needed (parallel eval enabled,
   this sink 'FlowSerial' -- see that function's haddock, which is where the
   actual deadlock-freedom argument lives: this lock is held only across
   that one call, during which no cap is evaluated and no promise is
   joined, so it never enters the promise table's wait-for graph). This is
   the CPS\/suspended sink path -- reached whenever a comp body's
   'Control.Computations.CompEngine.Types.compSinkReq' isn't part of a wider
   'CompReqCombined' batch; 'doCompSinkReqValue' just below is its
   'CompReqCombined'\/value-preparation counterpart, and needs the identical
   lock for the identical reason.
  -}
  doCompSinkReq
    :: forall x a s
     . (CompSink s)
    => CompMEnv
    -> TypedCompSinkId s
    -> CompSinkReq s a
    -> CompCont (Fail a) x
    -> CompEngineM (CompResult x)
  doCompSinkReq env sinkId req cont = do
    reg <- CompEngineM (asks (ce_compFlowRegistry . ce_compEngineIfs))
    parEnabled <- parEnabledM
    let sinkFun sink = do
          (outputs, res) <-
            liftIO $
              withSinkInstLock reg parEnabled (unTypedCompSinkId sinkId) (compSinkConcurrency sink) $
                compSinkExecute sink req
          return (wrapCompSinkOuts sink outputs, pure res)
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

  {- | The single evaluate-or-look-up site for a cap: every one of a nested
   eval's three shapes -- suspended CPS ('doAnyEvalReq'), a forked batch leaf
   ('prepEvalLeaf'), and a value prepared for a non-batch 'CompReqEval' -- all
   route through here, precisely so a parallel-evaluation project only has
   to wrap one function to cover all three.

   At 'ce_par' = 'Nothing' this is exactly 'evalWithCacheValue False innerCap
   (doCompAp innerCap)', the whole of what this function used to be. At
   'Just', every reference to @innerCap@ -- cache hit or miss alike -- goes
   through the promise table first (see 'claimOrJoin'): the first caller to
   reach a given cap becomes its "owner" and does the real
   'evalWithCacheValue' call (so a cache hit is still just a cache hit, only
   now published to any concurrent joiner too); every other caller for the
   *same* cap while that's in flight becomes a "joiner" and blocks on the
   owner's cell instead of re-entering the cache/state-if layer at all -- see
   'prepEvalLeaf's own note on why this turns a repeated 'hashCaching'
   reference within one batch from two evaluations into one, now that leaves
   can actually run concurrently.

   __Claim on start, never in the collect phase.__ This function runs
   exactly when a leaf's 'CompEngineM' action is actually reached (the "run
   phase", in 'Prep'\'s own terms) -- never during 'prepLeaf'\'s IO-layer
   traversal, which only *decides* whether to fork (see 'prepEvalLeaf'). If a
   batch instead claimed every eval leaf up front, @[eval A, eval B]@ where
   A's body needs B would deadlock with no dependency cycle anywhere in the
   graph: A would already own itself before it even started, and B's
   evaluation (needing A) would have no owner to wait on. Claiming only when
   a thread genuinely begins evaluating keeps every wait-for edge (a joiner
   blocking on `readMVar`) a real "joiner depends on owner" dependency edge
   -- so the wait-for graph is always a subgraph of the dependency graph, and
   __any deadlock is therefore a genuine cycle__, not an artifact of this
   scheduling choice. That is also why no separate cross-thread cycle
   detector exists here: a genuine cycle manifests as a set of threads
   mutually blocked on each other's 'MVar's with no path to being unblocked,
   which GHC's own indefinite-block detector already turns into a
   'Control.Exception.BlockedIndefinitelyOnMVar' rather than a silent hang --
   'doCompAp'\'s same-thread 'EvalChain' check (see its haddock) catches the
   common, single-threaded case immediately and with a precise message; this
   argument is what makes leaving the rarer cross-thread case to the runtime
   safe rather than merely convenient.
  -}
  doAnyEvalReqValue
    :: forall x
     . IsCompResult x
    => CompAp x
    -> CompEngineM (Maybe (CompApResult x))
  doAnyEvalReqValue innerCap =
    CompEngineM (asks ce_par) >>= \case
      Nothing -> plainEval
      Just par -> doAnyEvalReqValuePar par
   where
    plainEval = evalWithCacheValue False innerCap (doCompAp innerCap)

    doAnyEvalReqValuePar :: ParState -> CompEngineM (Maybe (CompApResult x))
    doAnyEvalReqValuePar par = do
      chain <- CompEngineM (asks ce_chain)
      let key = AnyCompAp innerCap
      claim <- liftIO (claimOrJoin par innerCap chain)
      case claim of
        ClaimOwner cell -> do
          logDebug ("doAnyEvalReqValue: claimed " ++ show key ++ " as owner (pool width " ++ show (ps_width par) ++ ")")
          outcome <- tryCompEngineM plainEval
          liftIO (putMVar cell outcome)
          liftIO (releaseClaim par key)
          either (liftIO . throwIO) pure outcome
        ClaimJoiner promise -> do
          logDebug
            ( "doAnyEvalReqValue: joining promise for "
                ++ show key
                ++ ", owner claimed it under chain "
                ++ show (HashSet.toList (ec_ancestors (ep_chain promise)))
            )
          -- Recorded here, before the 'readMVar' below rather than after --
          -- this is the moment this reference has *become* a joiner (the
          -- owner's claim is still live in the promise table, or this
          -- 'ClaimJoiner' couldn't exist), not the moment it wakes back up.
          -- See 'ed_promiseAwaits'\'s haddock: a caller polling this counter
          -- to detect "a joiner has shown up" needs it bumped at claim time,
          -- not at resolution time, or the poll would only ever observe it
          -- *after* the owner it's trying to unblock already had.
          ce <- CompEngineM (asks id)
          liftIO (whenDiag ce recordPromiseAwait)
          liftIO (writeIORef (ec_awaiting chain) (Just key))
          outcome <-
            liftIO $
              readMVar (ep_cell promise)
                `Control.Exception.finally` writeIORef (ec_awaiting chain) Nothing
          either (liftIO . throwIO) pure outcome

  doAnyCacheReqValue
    :: forall x
     . IsCompResult x
    => CompAp x
    -> CompEngineM (Maybe x)
  doAnyCacheReqValue innerCap =
    fmap (fmap cr_returnValue) (evalWithCacheValue True innerCap (return Nothing))

  -- | The 'CompReqCombined'\/value-preparation counterpart of
  -- 'doCompSinkReq' -- same body, minus the trailing 'loop', per this
  -- module's usual "Value" convention (see the note above
  -- 'doAnyEvalReqValue'). See 'doCompSinkReq'\'s own haddock for the
  -- locking\/deadlock-freedom argument, which applies here unchanged.
  doCompSinkReqValue
    :: forall a s
     . CompSink s
    => CompMEnv
    -> TypedCompSinkId s
    -> CompSinkReq s a
    -> CompEngineM (CompM (Fail a))
  doCompSinkReqValue env sinkId req = do
    reg <- CompEngineM (asks (ce_compFlowRegistry . ce_compEngineIfs))
    parEnabled <- parEnabledM
    let sinkFun sink = do
          (outputs, res) <-
            liftIO $
              withSinkInstLock reg parEnabled (unTypedCompSinkId sinkId) (compSinkConcurrency sink) $
                compSinkExecute sink req
          return (wrapCompSinkOuts sink outputs, pure res)
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
    -> Bool
    -- ^ whether this engine has parallel eval enabled -- threaded straight
    -- through to 'getOrCreateGroup' (see 'sg_lock').
    -> IORef [(CompSrcInstIx, SomeSrcGroup)]
    -> TypedCompSrcId s
    -> CompSrcReq s a
    -> Prep (Fail a)
  prepSrcLeaf reg parEnabled groupsRef sid req =
    Prep $
      withTypedCompSrcIdIndexed reg sid (\ix src -> pure (ix, src, compSrcConcurrency src)) >>= \case
        Fail reason ->
          let msg = "Refusing to run request for data source " ++ show sid ++ ": " ++ reason
           in pure (pure (return (Fail msg)))
        Ok (ix, src, conc) -> do
          slot <- newIORef Nothing
          g <- getOrCreateGroup reg parEnabled groupsRef ix src conc
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

  {- | 'prepLeaf'\'s 'CompLeafEval' branch: the collect-phase decision of
   whether to fork this leaf's evaluation, mirroring 'prepSrcLeaf'\'s own
   collect\/run split (see 'Prep'\'s haddock) -- but, unlike before this
   window existed, a leaf that loses the permit race here is no longer
   condemned to run inline for the rest of the batch's life. It is queued
   into the batch's shared 'PendingEvalLeaf' window (@mWindow@) instead, and
   gets more chances to start later, during the run phase, as earlier
   forks finish and free their permits back -- see 'topUpWindow'\'s haddock
   for why this was needed (in short: a batch far wider than the permit
   pool used to have its fork-vs-inline fate for *every* leaf sealed during
   this fast collect-time walk, before any of the earliest forks had even
   had a chance to finish -- see @docs\/benchmark-notes.md@'s Stage 13/14).

   __The first attempt still happens here, during collect, deliberately__ --
   this function does not defer *every* fork decision to the run phase; it
   only refuses to let a lost race be *final*. Reusing the original timing
   for a leaf's first attempt is what keeps a narrow batch (few eligible
   leaves, permits to spare) behaving exactly as it did before this window
   existed, which matters beyond just performance: a batch referencing the
   same 'hashCaching' cap twice depends on its second reference's fork
   actually racing the first reference's still-in-flight evaluation (see
   'doAnyEvalReqValue') to ever find a live promise to join instead of
   re-claiming as a fresh owner, and that race is only winnable if the
   second reference starts during collect, before the first reference's own
   run-phase join action has run to completion and released its claim.
   Deferring every leaf's *first* attempt to its own join point (as
   'topUpWindow' alone would) loses that race by construction for a batch's
   first eligible leaf, since nothing tops the window up before it ever
   runs -- caught directly by
   "Control.Computations.CompEngine.Tests.TestCompEvalConcurrency"'s
   @test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8@,
   which is what this design was built against.

   __Never forks the batch's first eval leaf__ (tracked via
   @firstEvalSeenRef@, a plain per-batch flag -- the traversal that calls
   this is itself sequential IO, so nothing here races another call to this
   same function): the calling thread always keeps at least one leaf's work
   for itself, so a single-eval-leaf batch never forks at all -- pure loss
   for a leaf with no sibling to overlap with, and the first eval leaf is
   never even added to the window.

   @mWindow@ is 'Nothing' exactly when 'ce_par' is 'Nothing' (see its
   allocation in 'doSuspended'\'s general path) -- both wildcarded together
   below, rather than unwrapped separately, so there is exactly one branch
   to reason about for "parallel eval is off", matching every other
   'ce_par'-gated site in this module.

   Every eligible leaf's own run-phase action:

    1. calls 'topUpWindow' -- this may or may not be what starts *this*
       leaf; it might already have been started by this function's own
       collect-time attempt above, by an earlier leaf's own run-phase call,
       or it might start several leaves beyond this one if enough permits
       are free right now (see 'topUpWindow'\'s haddock for why there is no
       cap on how far ahead a single call can reach);
    2. reads its own private @stateRef@: 'Just' means it is already forked
       (from the collect-time attempt or a later 'topUpWindow' call), so
       this just 'Async.wait's and merges, exactly as the old collect-time
       fork's run phase always did; 'Nothing' means every attempt so far
       has lost the race (by 'topUpWindow'\'s own invariant, this leaf is
       therefore still the queue's front), so it pops itself off and falls
       back to the same un-forked 'doAnyEvalReqValue' call this leaf would
       have run before this window existed -- Cilk's work-first serial
       elision, same guarantee as before, just no longer sealed after only
       one attempt.

   __Only *starts* move; *joins* do not.__ A leaf can be started well ahead
   of wherever the run phase currently is (at collect time, or by a later
   leaf's own 'topUpWindow' call), but every leaf's own join (the
   'Async.wait' or the inline fallback) still happens at that leaf's own
   position in 'Prep'\'s left-to-right 'Applicative' sequence, completely
   unchanged from before -- this is what preserves the leftmost-failing-leaf
   guarantee ('prepSrcLeaf'\'s own note makes the same argument for source
   leaves).

   __'tryAcquirePermit' only, never block__ (inside 'PendingEvalLeaf'\'s
   @pel_tryStart@, built here): a permit that could block would add an edge
   to the wait-for graph with no real dependency behind it -- see
   'doAnyEvalReqValue'\'s haddock on why that graph staying a subgraph of
   the dependency graph is what makes a genuine deadlock always a genuine
   cycle.

   On success, the forked action runs under
   "Control.Computations.Utils.ConcUtils".'trySync' (so an asynchronous
   cancellation -- e.g. this whole batch being torn down because an earlier
   sibling leaf failed, see 'doSuspended'\'s use of 'finallyCompEngineM' --
   is rethrown rather than swallowed, letting 'Async.cancel' actually stop
   this thread instead of leaving it to keep running the cap's body
   regardless) and a __fresh__ chain (this leaf's own ancestors seeded from
   the forking thread's, but its own 'ec_awaiting' slot -- see
   'EvalChain'\'s haddock). Whichever leaf ends up joining it merges
   whatever 'GenDel' the fork accumulated into its own via 'mergeGenDel'
   (the commutative-merge machinery 'CapSeq'\'s haddock documents, built for
   exactly this), and rethrows the fork's own exception unchanged on
   failure -- 'Async.wait'\'s own semantics.
  -}
  prepEvalLeaf
    :: forall x
     . IsCompResult x
    => CompEngine
    -> IORef Bool
    -> IORef [Async.Async ()]
    -> Maybe (IORef ([PendingEvalLeaf] -> [PendingEvalLeaf]), IORef [PendingEvalLeaf])
    -> CompAp x
    -> Prep (Maybe (CompApResult x))
  prepEvalLeaf ce firstEvalSeenRef forksRef mWindow cap = Prep $ do
    seenBefore <- atomicModifyIORef' firstEvalSeenRef (\seen -> (True, seen))
    let inline = pure <$> doAnyEvalReqValue cap
    case (ce_par ce, mWindow) of
      (Just par, Just (windowBuildRef, windowRef)) | seenBefore && rtsSupportsBoundThreads -> do
        stateRef <- newIORef Nothing
        let tryStart :: IO Bool
            tryStart = do
              whenDiag ce recordForkAttempt
              acquired <- tryAcquirePermit (ps_permits par)
              whenDiag ce (\d -> recordForkOutcome d acquired)
              when acquired $ do
                -- Depth is the *forking* thread's own chain size (the
                -- forked child sits one level deeper) -- see
                -- 'EngineDiag'\'s haddock. Occupancy is bumped here, at
                -- the moment the permit is actually taken (matching
                -- 'tryAcquirePermit' itself, just above), and unwound
                -- inside the forked action's own 'finally' below -- one
                -- timestamp per acquire, one per release, nothing
                -- allocated beyond that on this path.
                whenDiag ce $ \d -> do
                  recordDepth d (HashSet.size (ec_ancestors (ce_chain ce)))
                  bumpOccupancy d 1
                awaitingRef <- newIORef Nothing
                let forkEngine = ce{ce_chain = (ce_chain ce){ec_awaiting = awaitingRef}}
                -- The permit is released here, inside the forked action
                -- itself (success, synchronous failure, or cancellation
                -- alike), rather than around the 'Async.wait' below --
                -- deliberately, and not an arbitrary style choice. Joins
                -- stay strictly left-to-right (see this function's own
                -- haddock), but a fork's *actual work* can finish well out
                -- of that order -- a fast leaf started right after a slow
                -- one is a routine occurrence under this graph's
                -- heterogeneous per-source latency tiers. Releasing at
                -- join time would hold that fast leaf's permit hostage for
                -- however long every leaf *before* it in join order takes
                -- to be reached, even though the work behind it finished
                -- long ago -- measured directly: an earlier version that
                -- released at join time showed *fewer* successful forks
                -- than the collect-time-only decision this window
                -- replaced, not more, because permits were tied up idling
                -- on already-finished work instead of being available for
                -- 'topUpWindow' to hand to the next queued leaf. Releasing
                -- as soon as the work itself completes decouples pool
                -- throughput from join-order latency entirely; the join
                -- below still happens in order, but 'Async.wait' on an
                -- already-finished 'Async.Async' just reads its result,
                -- no different from before.
                asyncHandle <-
                  forkTracked forksRef $
                    trySync (runCompEngineM' (doAnyEvalReqValue cap) forkEngine)
                      `Control.Exception.finally` ( releasePermit (ps_permits par)
                                                       >> whenDiag ce (\d -> bumpOccupancy d (-1))
                                                   )
                writeIORef stateRef (Just asyncHandle)
              pure acquired
        -- Attempt immediately, right here during collect -- deliberately
        -- the *same* timing the old collect-time-only decision gave every
        -- leaf, not something this window replaces. Only a leaf that loses
        -- this first race gets queued for a later retry (see
        -- 'topUpWindow'); one that wins is forked at exactly the point in
        -- the traversal it always was. This is what keeps a narrow batch
        -- (few eligible leaves, permits to spare) behaving identically to
        -- before this window existed -- in particular, it is what a
        -- repeated 'hashCaching' reference's promise-table dedup depends
        -- on: the second reference's fork has to actually be racing the
        -- first reference's still-in-flight evaluation (see
        -- 'doAnyEvalReqValue'), and it only gets that race if it starts
        -- during collect, before the first reference's own run-phase join
        -- action has had a chance to run to completion and release its
        -- claim. Deferring every leaf's first attempt to its own join
        -- point (as 'topUpWindow' alone would) loses that race by
        -- construction for a batch's first eligible leaf, since nothing
        -- ever tops the window up *before* it -- see
        -- "Control.Computations.CompEngine.Tests.TestCompEvalConcurrency"'s
        -- @test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8@,
        -- which is what caught exactly that regression.
        startedHere <- tryStart
        unless startedHere $
          modifyIORef' windowBuildRef (\fs -> fs . (PendingEvalLeaf tryStart :))
        pure $ do
          liftIO (topUpWindow windowRef)
          mHandle <- liftIO (readIORef stateRef)
          case mHandle of
            Just asyncHandle -> do
              -- No 'finally' needed here: the permit (and its occupancy
              -- bump) is already released inside the forked action itself,
              -- the moment its own work actually finishes -- see the
              -- comment above 'forkTracked's call, just above. This
              -- 'Async.wait' only ever reads a result; on an
              -- already-finished 'Async.Async' (the common case once the
              -- window is running well ahead of the join cursor) it
              -- returns immediately.
              outcome <- liftIO (Async.wait asyncHandle)
              case outcome of
                Left ex -> liftIO (throwIO ex)
                Right (mres, genDel) -> mergeGenDel genDel >> pure (pure mres)
            Nothing -> do
              liftIO (popWindowFront windowRef)
              inline
      _ -> pure inline

  {- | Prepare one leaf of a 'CompReqCombined' batch as a 'Prep' value.
   'CompLeafSrc' is the only leaf kind that ever joins a 'SrcGroup';
   'CompLeafEval' is the only kind that may fork (see 'prepEvalLeaf'); every
   other kind always runs inline, exactly as before batching existed.
  -}
  prepLeaf
    :: forall r
     . CompMEnv
    -> CompEngine
    -> CompFlowRegistry
    -> IORef [(CompSrcInstIx, SomeSrcGroup)]
    -> IORef Bool
    -> IORef [Async.Async ()]
    -> Maybe (IORef ([PendingEvalLeaf] -> [PendingEvalLeaf]), IORef [PendingEvalLeaf])
    -> CompReqLeaf r
    -> Prep r
  prepLeaf env ce reg groupsRef firstEvalSeenRef forksRef mWindow leaf =
    case leaf of
      CompLeafEval cap -> prepEvalLeaf ce firstEvalSeenRef forksRef mWindow cap
      CompLeafCache cap -> Prep (pure (pure <$> doAnyCacheReqValue cap))
      CompLeafSrc sid req -> prepSrcLeaf reg (isJust (ce_par ce)) groupsRef sid req
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
        -- Fetched here, before the fast-path/general-path fork below, so
        -- both branches can share it: the fast path needs it only for the
        -- leaf-count histogram just below (never for forking -- see its own
        -- comment), the general path for the same reasons it always did
        -- (see its own comment on 'firstEvalSeenRef'/'forksRef').
        ce <- CompEngineM (asks id)
        -- 'req' here is still this whole 'CompReqCombined' node (the case
        -- match below only destructures it, doesn't shadow it) -- this is
        -- the single site every batch, fast-path or general, passes through
        -- exactly once, so counting here counts every batch exactly once,
        -- fast path included, regardless of 'ce_par' entirely (see
        -- 'EngineDiag'\'s haddock on why this histogram doesn't gate on
        -- it). 'countEvalLeaves' is a pure walk and 'whenDiag' never forces
        -- it when diagnostics are off, so this costs nothing then.
        liftIO (whenDiag ce (\d -> recordLeafCount d (countEvalLeaves req)))
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
            -- firstEvalSeenRef/forksRef: per-batch bookkeeping for eval-leaf
            -- forking (see 'prepEvalLeaf'). @ce@ is fetched once here
            -- (rather than re-'asks' per leaf) so every forked leaf shares
            -- the exact same 'ce_par'/'ce_compEngineIfs'/'ce_capSeqCounter'
            -- by reference, only ever varying 'ce_chain'.
            firstEvalSeenRef <- liftIO (newIORef False)
            forksRef <- liftIO (newIORef [])
            -- mWindow: the batch's shared sliding fork window (see
            -- 'PendingEvalLeaf'/'topUpWindow'), allocated only when
            -- parallel eval is actually on -- 'ce_par' being 'Nothing'
            -- means 'prepEvalLeaf' never looks at it (its wildcarded
            -- '_ -> pure inline' branch matches before ever touching
            -- @mWindow@), so there is no reason to pay for two more
            -- 'IORef's on a path that can never fork. This keeps a
            -- width-1 (or non-bound-threads) general-path batch exactly as
            -- allocation-light as it was before this window existed --
            -- see 'CompEngine'\'s own 'ce_par' haddock on why that bar is
            -- non-negotiable. @windowBuildRef@ is the collect-phase
            -- difference-list accumulator (same O(1)-append idiom
            -- @sg_fetches@ uses); it is read out into @windowRef@ -- the
            -- actual queue 'topUpWindow' consumes -- exactly once, right
            -- after the traversal below finishes, mirroring how @groups@
            -- is read out of @groupsRef@ just below.
            mWindow <-
              if isJust (ce_par ce)
                then liftIO (Just <$> ((,) <$> newIORef id <*> newIORef []))
                else pure Nothing
            enginePhase <-
              liftIO
                ( unPrep
                    (traverseCompReq (prepLeaf env ce reg groupsRef firstEvalSeenRef forksRef mWindow) req)
                )
            -- Read out into the actual queue 'topUpWindow' consumes,
            -- exactly once, right after the traversal above finishes,
            -- mirroring how @groups@ is read out of @groupsRef@ just
            -- below. By this point @windowRef@ only ever holds a leaf
            -- whose own collect-time 'tryStart' attempt (see
            -- 'prepEvalLeaf') already lost the permit race -- every leaf
            -- that won it is already forked, exactly as it always was.
            liftIO $ forM_ mWindow $ \(windowBuildRef, windowRef) -> do
              dl <- readIORef windowBuildRef
              writeIORef windowRef (dl [])
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
            -- Join every eval-leaf fork this batch started before returning,
            -- including on the exception path -- 'cancelAllTracked' is a
            -- no-op for any fork already individually joined by its own
            -- leaf (see 'prepEvalLeaf') and tears down the rest, the same
            -- guarantee 'Async.withAsync' gives for a *fixed* fork count,
            -- reconstructed here for a dynamic one. This is also what keeps
            -- a fork from outliving the batch and racing 'allCompSrcChanges'
            -- (Impl.hs's own three-reason note above) the same way a
            -- dispatched source job already can't.
            inner <- finallyCompEngineM enginePhase (cancelAllTracked forksRef)
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

{- | Build a fresh 'CompEngine', including reading
 'CompFlowRegistry.readCompEvalConcurrency' __exactly once__ to decide
 'ce_par' -- see that function's haddock for why a later
 'CompFlowRegistry.setCompEvalConcurrency' call has no effect on an
 already-initialised engine, unlike the source-side width knob. A width of 1
 (the registry's own default, and every pre-existing caller's behaviour)
 gives 'Nothing': no promise table, no permit pool, allocated for nothing,
 and every gated code path this project added takes its old, unmodified
 branch -- see 'CompEngine'\'s own haddock on 'ce_par' for why that is the
 acceptance bar this function exists to guarantee.
-}
initCompEngine :: CompEngineIfs -> IO CompEngine
initCompEngine compEngineIfs = do
  counter <- newIORef 0
  width <- unCompFlowConcurrency <$> readCompEvalConcurrency (ce_compFlowRegistry compEngineIfs)
  mPar <-
    if width <= 1
      then pure Nothing
      else do
        promises <- newIORef HashMap.empty
        permits <- newTVarIO (width - 1)
        pure (Just (ParState promises permits width))
  awaitingRoot <- newIORef Nothing
  let chain = EvalChain{ec_ancestors = HashSet.empty, ec_awaiting = awaitingRoot}
  -- See 'EngineDiag'\'s haddock: read independently of 'width' above, so
  -- 'ce_diag' is set purely by the env var, never coupled to 'ce_par'.
  diagOn <- lockStatsEnabled
  mDiag <- if diagOn then Just <$> newEngineDiag else pure Nothing
  return (CompEngine compEngineIfs counter mPar chain mDiag)

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

{- | Logs, then prints this engine's 'EngineDiag' tables (a no-op when
 @COMP_ENGINE_LOCK_STATS@ was off -- see 'reportEngineDiag'). This
 module's only caller ("Control.Computations.CompEngine.Run"'s @main@)
 wraps its call to this function in a 'Control.Exception.finally' around
 the run loop, so this runs on every teardown path, including
 'Control.Concurrent.Async.cancel' -- the same guarantee
 'Run.setupSimpleStateIf's @reportLockStats@ already relies on (see its
 own haddock in "Run.hs"), extended here to cover this function too.
-}
stopCompEngine :: CompEngine -> IO ()
stopCompEngine compEngine = do
  flip evalCompEngineM compEngine $
    logNote "CompEngine stopped."
  reportEngineDiag compEngine
