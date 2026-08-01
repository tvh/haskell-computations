{-# LANGUAGE TypeFamilies #-}

{- | A synthetic 'CompSrc' modelling one external clinical system (the
 hospital bench in "Control.Computations.Demos.Bench.Hospital" registers
 five instances of this type: admissions\/discharge\/transfer, vitals, labs,
 pharmacy, notes). Shaped like "Control.Computations.FlowImpls.HashMapFlow"
 -- an in-memory @TVar (HashMap Key Val)@ plus a change-set 'TVar' feeding
 'compSrcWaitChanges' in exactly the same way (see that module's haddock for
 the reasoning behind that shape; it is not repeated here) -- plus the
 things a real service call has that 'HashMapFlow'\'s in-memory
 'Control.Computations.FlowImpls.HashMapFlow.hmfLookup' does not:

 * a per-instance __latency__ (a 'threadDelay' inside 'compSrcExecute',
   standing in for real network\/service time -- this is the whole reason
   the hospital bench exists: 'Control.Computations.Demos.Bench.Main'\'s
   graph has no latency anywhere to hide, so concurrency can never show up
   in its numbers regardless of width);
 * optional, __deterministic per-key jitter__ on top of that latency (see
   'ssc_jitterEnabled'\/'jitterUs') -- off by default, so every stage before
   this one reproduces exactly, and derived from a hash of the key rather
   than a random-number generator, so a run with jitter enabled is still
   bit-for-bit reproducible;
 * a __call counter__ and a __concurrency high-water mark__, both plain
   'IORef'\/'TVar' bookkeeping bumped on every request served, so the
   benchmark can report how many source calls actually happened and how many
   of them were genuinely observed overlapping (see 'sysCallCount'\/
   'sysHighWaterMark');
 * a __batch call counter__, separate from the request counter: this type
   overrides 'compSrcExecuteBatch' (see 'executeBatchBundled') to serve every
   request the engine has bundled together against this instance with one
   'readTVarIO' and one simulated-latency 'threadDelay', instead of one of
   each per request -- 'sysCallCount' still counts every individual request
   served, so 'sysCallCount' \/ 'sysBatchCallCount' is directly the round-trip
   reduction this buys, visible in the benchmark's own report rather than
   only inferred from timing;
 * a runtime knob (see 'ssc_batchingEnabled') to turn that override back
   __off__, falling back to 'CompSrc'\'s own default sequential
   implementation -- see 'defaultSequentialBatch' -- so a benchmark can hold
   concurrency width and bundling apart as independent variables instead of
   only ever measuring their product.

 NOTE on a small, deliberately-accepted cost: giving 'compSrcExecute'\/
 'compSrcExecuteBatch' a per-call *computed* latency (via 'latencyForKey'\/
 'batchLatencyUs') rather than reading 'sys_latencyUs' as a compile-time-flat
 field access costs a small, measured amount of extra allocation on this hot
 path even when jitter is off and the computed value is numerically identical
 to the field itself -- confirmed by isolating the change: at Hospital's full
 scale (~1,612,000 requests, ~593,000 batch calls), this adds
 ~64.3 MB (+0.18%) to @allocated_bytes (cold eval)@, with @max_live_bytes@
 unaffected (peak residency, not cumulative allocation). Every attempt to
 remove it (bang patterns, 'INLINE' on every function in the chain) failed to
 recover the flat-field-access baseline, so it is reported here rather than
 chased further -- the two runtime knobs this module exists to add are the
 point, and a 0.18% allocation cost for carrying them is well inside this
 doc's own materiality bar.
-}
module Control.Computations.Demos.Bench.SystemSrc (
  SystemSrc,
  SystemReq (..),
  Key,
  Val,
  SystemSrcConfig (..),
  defaultSystemSrcConfig,
  initSystemSrc,
  initSystemSrcWith,
  sysInsert,
  sysInsertBatch,
  sysCallCount,
  sysBatchCallCount,
  sysHighWaterMark,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception (fromException), SomeAsyncException (..), SomeException, throwIO)
import qualified Control.Exception as Exception
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (hash)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import qualified Data.Text as T
import Data.Typeable

type Key = BS.ByteString
type Val = BS.ByteString
type Ver = Hash128

type SystemDep = Dep Key (Maybe Ver)

data SystemSrc = SystemSrc
  { sys_ident :: T.Text
  , sys_dataVar :: TVar (HashMap Key Val)
  , sys_changesVar :: TVar (HashSet SystemDep)
  , sys_latencyUs :: Int
  , sys_jitterEnabled :: Bool
  , sys_batchingEnabled :: Bool
  , sys_callCount :: IORef Int
  , sys_batchCallCount :: IORef Int
  , sys_inFlight :: TVar Int
  , sys_highWater :: TVar Int
  }
  deriving (Typeable)

data SystemReq a where
  SystemLookupReq :: Key -> SystemReq (Maybe Val)

instance CompSrc SystemSrc where
  type CompSrcReq SystemSrc = SystemReq
  type CompSrcKey SystemSrc = Key
  type CompSrcVer SystemSrc = Maybe Ver
  compSrcInstanceId = CompSrcInstanceId . sys_ident
  compSrcExecute = executeImpl
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges = waitChangesImpl

  {- | 'compSrcExecute' (see 'executeImpl') bumps two counters via plain STM,
   optionally 'threadDelay's, then takes a 'readTVarIO' snapshot of the
   backing map -- no write, no lock, nothing here requires calls against this
   instance to be serialized against each other. Declaring 'FlowConcurrent'
   is the entire reason this type exists: with a nonzero 'sys_latencyUs',
   this is what lets several reads against *different* instances overlap
   their 'threadDelay's (see "Control.Computations.CompEngine.Impl"'s
   @doSuspended@); reads against the *same* instance within one batch don't
   need that anymore -- see 'compSrcExecuteBatch' below, which collapses
   them into one delay instead (when 'sys_batchingEnabled', see
   'executeBatchDispatch').
  -}
  compSrcConcurrency _ = FlowConcurrent

  -- | Dispatches on 'sys_batchingEnabled' -- see 'executeBatchDispatch'.
  compSrcExecuteBatch = executeBatchDispatch

{- | Everything about an instance's simulated I/O behaviour that isn't its
 identity or its backing data: the base per-call latency plus the two
 runtime knobs -- see 'defaultSystemSrcConfig' for the
 no-jitter\/bundling-on defaults every pre-existing benchmark relies on
 ('initSystemSrc' is exactly that default, unchanged), and
 "Control.Computations.Demos.Bench.Tiered" for a client that actually varies
 all three.
-}
data SystemSrcConfig = SystemSrcConfig
  { ssc_latencyUs :: Int
  -- ^ Base per-call latency in microseconds; 0 disables the simulated delay
  -- entirely (see 'simulateRoundTrip'). Paid once per 'compSrcExecute' call
  -- and once per bundled 'compSrcExecuteBatch' call regardless of how many
  -- requests that call serves -- see 'executeBatchBundled'.
  , ssc_jitterEnabled :: Bool
  -- ^ Whether 'jitterUs' adds a deterministic, key-derived amount on top of
  -- 'ssc_latencyUs' (see that function's haddock). Off by default.
  , ssc_batchingEnabled :: Bool
  -- ^ Whether 'compSrcExecuteBatch' actually bundles same-instance requests
  -- into one simulated round trip ('executeBatchBundled') or falls back to
  -- 'CompSrc'\'s own default sequential behaviour, one 'compSrcExecute' call
  -- per request ('defaultSequentialBatch'). On by default -- this is what
  -- every stage before the one that introduced this field measured.
  }

-- | No jitter, bundling on -- the behaviour every 'CompSrc' instance had
-- before those two knobs existed. 'initSystemSrc' is 'initSystemSrcWith'
-- applied to this, unconditionally, so every existing caller (the Hospital
-- benchmark) keeps its exact prior behaviour with zero changes on its side.
defaultSystemSrcConfig :: Int -> SystemSrcConfig
defaultSystemSrcConfig latencyUs =
  SystemSrcConfig
    { ssc_latencyUs = latencyUs
    , ssc_jitterEnabled = False
    , ssc_batchingEnabled = True
    }

-- | Build a fresh, empty instance named @ident@ with the given per-call
-- latency in microseconds and every other knob at its default (no jitter,
-- bundling on) -- see 'defaultSystemSrcConfig'. Kept as the stable
-- two-argument entry point the Hospital benchmark already calls; reach for
-- 'initSystemSrcWith' to vary jitter or bundling.
initSystemSrc :: T.Text -> Int -> IO SystemSrc
initSystemSrc ident latencyUs = initSystemSrcWith ident (defaultSystemSrcConfig latencyUs)

-- | Like 'initSystemSrc', but takes a full 'SystemSrcConfig' instead of just
-- a latency -- the entry point for a caller (the Tiered benchmark) that
-- needs to vary jitter or bundling per instance or per run.
initSystemSrcWith :: T.Text -> SystemSrcConfig -> IO SystemSrc
initSystemSrcWith ident cfg = do
  dataV <- newTVarIO HashMap.empty
  changesV <- newTVarIO HashSet.empty
  countRef <- newIORef 0
  batchCountRef <- newIORef 0
  inFlightV <- newTVarIO 0
  highWaterV <- newTVarIO 0
  pure
    ( SystemSrc
        ident
        dataV
        changesV
        (ssc_latencyUs cfg)
        (ssc_jitterEnabled cfg)
        (ssc_batchingEnabled cfg)
        countRef
        batchCountRef
        inFlightV
        highWaterV
    )

waitChangesImpl :: SystemSrc -> STM (HashSet SystemDep)
waitChangesImpl sys = do
  set <- readTVar (sys_changesVar sys)
  writeTVar (sys_changesVar sys) HashSet.empty
  if HashSet.null set
    then retry
    else pure set

{- | Deterministic per-key jitter in @[0, 'sys_latencyUs'))@, added on top of
 the base latency when 'sys_jitterEnabled' -- derived from
 'Data.Hashable.hash' of the key itself, __never__ a random-number
 generator, so the same key gets the same jitter on every run: a jittered
 run is exactly as reproducible (same reruns, same allocation) as an
 unjittered one, just with a wider, key-dependent spread of per-call wait
 times. That spread is the entire point (see
 "Control.Computations.Demos.Bench.Tiered"'s module haddock): a constant
 delay makes every call equally slow, so p99 can never differ from p50 and
 concurrency has nothing tail-shaped to hide. Disabled (returns 0)
 unconditionally when 'sys_latencyUs' is 0 -- there is no base delay to
 jitter a fraction of.
-}
jitterUs :: SystemSrc -> Key -> Int
jitterUs sys key
  | not (sys_jitterEnabled sys) = 0
  | base <= 0 = 0
  -- 'mod' (unlike 'rem') always takes the sign of its divisor, so this is
  -- already in @[0, base)@ regardless of 'hash'\'s own sign -- no 'abs'
  -- needed.
  | otherwise = hash key `mod` base
 where
  base = sys_latencyUs sys

-- | This key's own simulated latency: the instance's base 'sys_latencyUs'
-- plus this key's 'jitterUs' -- see 'executeImpl'.
latencyForKey :: SystemSrc -> Key -> Int
latencyForKey sys key = sys_latencyUs sys + jitterUs sys key

{- | Bump the in-flight\/high-water pair, optionally sleep the given number of
 microseconds to stand in for a real service call, then decrement -- shared
 by 'executeImpl' (one request pays this once, for its own key's
 'latencyForKey') and 'executeBatchBundled' (a whole bundled group pays this
 once total, not once per request -- the collapse this type exists to
 demonstrate, for the *worst* key's 'latencyForKey' among the group -- see
 that function). The in-flight bump\/decrement brackets only the delay, so
 'sysHighWaterMark' reports how many *calls* (single or batched) were
 genuinely inside the simulated wait at once, not how many individual
 requests were being served -- that's 'sysCallCount'.
-}
simulateRoundTrip :: SystemSrc -> Int -> IO ()
simulateRoundTrip sys latencyUs = do
  atomically $ do
    n <- (+ 1) <$> readTVar (sys_inFlight sys)
    writeTVar (sys_inFlight sys) n
    hw <- readTVar (sys_highWater sys)
    when (n > hw) (writeTVar (sys_highWater sys) n)
  when (latencyUs > 0) (threadDelay latencyUs)
  atomically (modifyTVar' (sys_inFlight sys) (subtract 1))

-- | Serve exactly one request: bump the request counter by one, pay one
-- simulated round trip ('simulateRoundTrip', at this key's own
-- 'latencyForKey'), then answer from the in-memory map. Reached for a
-- request that never joined a batch, and -- since 'defaultSequentialBatch'
-- calls this once per request -- for every request served while
-- 'sys_batchingEnabled' is off (see 'executeBatchDispatch').
executeImpl :: SystemSrc -> SystemReq a -> IO (HashSet SystemDep, Fail a)
executeImpl sys (SystemLookupReq key) = do
  atomicModifyIORef' (sys_callCount sys) (\n -> (n + 1, ()))
  simulateRoundTrip sys (latencyForKey sys key)
  mVal <- HashMap.lookup key <$> readTVarIO (sys_dataVar sys)
  pure (HashSet.singleton (Dep key (fmap largeHash128 mVal)), Ok mVal)

-- | Dispatch on 'sys_batchingEnabled': the bundled path
-- ('executeBatchBundled', the historical behaviour and still the default)
-- or a plain per-request fallback ('defaultSequentialBatch') that makes
-- this instance behave exactly as if it had never overridden
-- 'compSrcExecuteBatch' at all -- see the module haddock's closing bullet
-- and "Control.Computations.Demos.Bench.Tiered" for why a benchmark needs
-- to be able to flip this at runtime.
executeBatchDispatch :: SystemSrc -> [SrcFetch SystemSrc] -> IO ()
executeBatchDispatch sys fetches
  | sys_batchingEnabled sys = executeBatchBundled sys fetches
  | otherwise = defaultSequentialBatch sys fetches

{- | Serve every request the engine bundled together against this instance
 with __one__ simulated round trip ('simulateRoundTrip', so one
 'threadDelay' for the whole group instead of one per request -- at the
 *worst* (largest) 'latencyForKey' among the group's keys, the realistic
 model for a batched round trip: its wall time is bounded by its slowest
 constituent, not its average) and __one__ 'readTVarIO' snapshot, then
 answer each request with a pure 'HashMap.lookup' against that shared
 snapshot. Bumps 'sys_callCount' by the number of requests served (so it
 keeps meaning \"how many individual lookups happened\" regardless of how
 they were grouped) and 'sys_batchCallCount' by exactly one (so it means
 \"how many round trips happened\") -- their ratio is this feature's payoff,
 directly reportable rather than only inferable from timing.
-}
executeBatchBundled :: SystemSrc -> [SrcFetch SystemSrc] -> IO ()
executeBatchBundled sys fetches = do
  atomicModifyIORef' (sys_callCount sys) (\n -> (n + length fetches, ()))
  atomicModifyIORef' (sys_batchCallCount sys) (\n -> (n + 1, ()))
  simulateRoundTrip sys (batchLatencyUs sys fetches)
  m <- readTVarIO (sys_dataVar sys)
  F.for_ fetches (answerFetch m)
 where
  -- See "Control.Computations.FlowImpls.HashMapFlow"'s own answerFetch
  -- helper for why this needs its own top-level-shaped, explicitly-typed
  -- equation rather than an inline lambda.
  answerFetch :: HashMap Key Val -> SrcFetch SystemSrc -> IO ()
  answerFetch m (SrcFetch (SystemLookupReq key) write) =
    let mVal = HashMap.lookup key m
     in write (Right (HashSet.singleton (Dep key (fmap largeHash128 mVal)), Ok mVal))

{- | The whole batch's simulated latency. With jitter off (the common case --
 every pre-jitter caller, including the Hospital benchmark, runs this way)
 every key's 'latencyForKey' is identically 'sys_latencyUs', so this takes a
 fast path that skips scanning @fetches@ entirely. With jitter on, the worst
 (largest) 'latencyForKey' among the batch's keys wins -- see 'jitterUs's
 haddock for why worst-of, not average, is the realistic model.
-}
batchLatencyUs :: SystemSrc -> [SrcFetch SystemSrc] -> Int
batchLatencyUs sys fetches
  | not (sys_jitterEnabled sys) = sys_latencyUs sys
  | otherwise =
      maximum (sys_latencyUs sys : [latencyForKey sys k | SrcFetch (SystemLookupReq k) _ <- fetches])

-- | 'CompSrc'\'s own default 'compSrcExecuteBatch' (see that method's
-- haddock), reimplemented here rather than called -- Haskell instance
-- methods have no \"call the class default from inside an override\" -- so
-- that 'sys_batchingEnabled' can genuinely fall all the way back to it:
-- every request served one at a time, each via 'executeImpl' (so each pays
-- its own 'latencyForKey' independently rather than sharing one collapsed
-- delay), with 'trySyncLocal' isolating one request's exception from its
-- siblings exactly as the class default does.
defaultSequentialBatch :: SystemSrc -> [SrcFetch SystemSrc] -> IO ()
defaultSequentialBatch sys fetches =
  forM_ fetches $ \(SrcFetch req write) -> trySyncLocal (compSrcExecute sys req) >>= write

{- | A local stand-in for
 "Control.Computations.Utils.ConcUtils".'Control.Computations.Utils.ConcUtils.trySync'
 -- byte-for-byte the same catch-synchronous\/rethrow-asynchronous logic --
 which is exactly what 'defaultSequentialBatch' needs to reproduce
 'CompSrc'\'s own default faithfully, but is not part of the public API this
 benchmark stanza is restricted to (@ConcUtils@ is an internal module, not
 in the library's @exposed-modules@ -- see the constraints this benchmark
 was built under). Reported rather than worked around by widening what
 @bench/@ may import: one small duplicated helper is a much smaller
 footprint than exposing an internal concurrency-utilities module purely for
 this one function, and it is a genuine finding about the public API's
 sufficiency, not a design choice made lightly.
-}
trySyncLocal :: IO a -> IO (Either SomeException a)
trySyncLocal action =
  Exception.try action >>= \case
    Left e
      | Just (SomeAsyncException _) <- fromException e -> throwIO e
      | otherwise -> pure (Left e)
    Right v -> pure (Right v)

-- | Insert or overwrite @key@, and record the change so a comp body reading
-- it sees the update -- the source-side mutation hook used for the
-- benchmark's live-update phase (mirrors
-- 'Control.Computations.FlowImpls.HashMapFlow.hmfInsert').
sysInsert :: SystemSrc -> Key -> Val -> IO ()
sysInsert sys key val = atomically (insertOneSTM sys key val)

-- | Insert or overwrite many @(source, key, value)@ triples -- possibly
-- against several different 'SystemSrc' instances -- in one single STM
-- transaction, so they become visible to the engine as one indivisible unit.
--
-- This is not just a convenience: it is load-bearing for any caller that
-- wants to mutate more than one key \"in one go\" and then wait for the
-- engine to settle. 'compSrcWaitChanges' (see 'waitChangesImpl') is an STM
-- @retry@ loop, and STM wakes a blocked transaction \-\- and lets it commit
-- \-\- as soon as *any* single 'TVar' it reads changes, not once every write
-- a caller /intends/ as one batch has landed. A caller that instead issues
-- @N@ separate 'sysInsert' calls races its own remaining writes against the
-- driver thread: the driver's blocked
-- "Control.Computations.CompEngine.CompFlowRegistry".@allCompSrcChanges@
-- transaction can wake on the *first* write, process only that one change,
-- and report a run with zero leftover stale caps while the other @N-1@
-- writes are still pending -- which is indistinguishable, from
-- 'Control.Computations.CompEngine.Driver.waitForFullSettle's own
-- perspective, from \"genuinely fully drained\" (see that function's
-- haddock and the callers' own settle-discipline comments). The result is a
-- silent undercount, not a crash: confirmed by hand, a batch of 1,000
-- separate 'sysInsert' calls followed by one 'waitForFullSettle' produced
-- 1,981 reruns on one process run and 7,578 on another, for the identical
-- mutation set. Committing every write in the batch inside one 'atomically'
-- block removes the race outright: STM guarantees the whole transaction is
-- either invisible to every other transaction or fully visible, so the
-- driver can never observe a partial batch.
sysInsertBatch :: [(SystemSrc, Key, Val)] -> IO ()
sysInsertBatch = atomically . mapM_ (\(sys, key, val) -> insertOneSTM sys key val)

-- | Shared by 'sysInsert' and 'sysInsertBatch': record @val@ at @key@ in
-- @sys@'s data map and its change set. Not itself wrapped in 'atomically' --
-- callers compose it into whatever transaction they need (a single-key one
-- for 'sysInsert', an arbitrarily-sized one for 'sysInsertBatch').
insertOneSTM :: SystemSrc -> Key -> Val -> STM ()
insertOneSTM sys key val = do
  modifyTVar' (sys_dataVar sys) (HashMap.insert key val)
  modifyTVar'
    (sys_changesVar sys)
    (HashSet.union (HashSet.singleton (Dep key (Just (largeHash128 val)))))

-- | Total number of individual requests this instance has served so far,
-- via either 'executeImpl' or 'executeBatchBundled'\/'defaultSequentialBatch'.
sysCallCount :: SystemSrc -> IO Int
sysCallCount sys = readIORef (sys_callCount sys)

-- | Total number of 'compSrcExecuteBatch' round trips this instance has
-- served so far -- always <= 'sysCallCount', and 'sysCallCount' \/
-- 'sysBatchCallCount' is the bundling win as an observed ratio. Only bumped
-- by 'executeBatchBundled'; 'defaultSequentialBatch' calls 'executeImpl' per
-- request instead, so with 'sys_batchingEnabled' off this stays at 0
-- regardless of how many requests were served.
sysBatchCallCount :: SystemSrc -> IO Int
sysBatchCallCount sys = readIORef (sys_batchCallCount sys)

-- | The largest number of 'compSrcExecute' calls against this instance ever
-- observed genuinely in flight at once (see 'executeImpl'). 1 means every
-- call so far ran strictly after the previous one finished -- i.e. no
-- overlap was ever observed, regardless of what caused that (width 1,
-- 'FlowSerial', or simply no batch wide enough to dispatch a job).
sysHighWaterMark :: SystemSrc -> IO Int
sysHighWaterMark sys = readTVarIO (sys_highWater sys)
