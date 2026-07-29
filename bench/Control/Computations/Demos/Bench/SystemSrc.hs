{-# LANGUAGE TypeFamilies #-}

{- | A synthetic 'CompSrc' modelling one external clinical system (the
 hospital bench in "Control.Computations.Demos.Bench.Hospital" registers
 five instances of this type: admissions\/discharge\/transfer, vitals, labs,
 pharmacy, notes). Shaped like "Control.Computations.FlowImpls.HashMapFlow"
 -- an in-memory @TVar (HashMap Key Val)@ plus a change-set 'TVar' feeding
 'compSrcWaitChanges' in exactly the same way (see that module's haddock for
 the reasoning behind that shape; it is not repeated here) -- plus the two
 things a real service call has that 'HashMapFlow'\'s in-memory
 'Control.Computations.FlowImpls.HashMapFlow.hmfLookup' does not:

 * a per-instance __latency__ (a 'threadDelay' inside 'compSrcExecute',
   standing in for real network\/service time -- this is the whole reason
   the hospital bench exists: 'Control.Computations.Demos.Bench.Main'\'s
   graph has no latency anywhere to hide, so concurrency can never show up
   in its numbers regardless of width);
 * a __call counter__ and a __concurrency high-water mark__, both plain
   'IORef'\/'TVar' bookkeeping bumped on every 'compSrcExecute' call, so the
   benchmark can report how many source calls actually happened and how many
   of them were genuinely observed overlapping (see 'sysCallCount'\/
   'sysHighWaterMark').
-}
module Control.Computations.Demos.Bench.SystemSrc (
  SystemSrc,
  SystemReq (..),
  Key,
  Val,
  initSystemSrc,
  sysInsert,
  sysCallCount,
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
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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
  , sys_callCount :: IORef Int
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
   this is what lets several reads against the same instance overlap their
   'threadDelay's instead of paying them one after another (see
   "Control.Computations.CompEngine.Impl"'s @prepSrcLeaf@).
  -}
  compSrcConcurrency _ = FlowConcurrent

-- | Build a fresh, empty instance named @ident@ with the given
-- per-'compSrcExecute'-call latency in microseconds (0 disables the delay
-- entirely -- see 'executeImpl').
initSystemSrc :: T.Text -> Int -> IO SystemSrc
initSystemSrc ident latencyUs = do
  dataV <- newTVarIO HashMap.empty
  changesV <- newTVarIO HashSet.empty
  countRef <- newIORef 0
  inFlightV <- newTVarIO 0
  highWaterV <- newTVarIO 0
  pure (SystemSrc ident dataV changesV latencyUs countRef inFlightV highWaterV)

waitChangesImpl :: SystemSrc -> STM (HashSet SystemDep)
waitChangesImpl sys = do
  set <- readTVar (sys_changesVar sys)
  writeTVar (sys_changesVar sys) HashSet.empty
  if HashSet.null set
    then retry
    else pure set

{- | Bump the call counter and the in-flight\/high-water pair (both cheap,
 independent of the simulated latency below), optionally sleep
 'sys_latencyUs' microseconds to stand in for a real service call, then
 answer from the in-memory map. The in-flight bump\/decrement brackets the
 delay specifically so 'sysHighWaterMark' reports how many callers were
 genuinely inside the simulated call at once, not merely how many calls
 happened in total (that's 'sysCallCount').
-}
executeImpl :: SystemSrc -> SystemReq a -> IO (HashSet SystemDep, Fail a)
executeImpl sys (SystemLookupReq key) = do
  atomicModifyIORef' (sys_callCount sys) (\n -> (n + 1, ()))
  atomically $ do
    n <- (+ 1) <$> readTVar (sys_inFlight sys)
    writeTVar (sys_inFlight sys) n
    hw <- readTVar (sys_highWater sys)
    when (n > hw) (writeTVar (sys_highWater sys) n)
  when (sys_latencyUs sys > 0) (threadDelay (sys_latencyUs sys))
  atomically (modifyTVar' (sys_inFlight sys) (subtract 1))
  mVal <- HashMap.lookup key <$> readTVarIO (sys_dataVar sys)
  pure (HashSet.singleton (Dep key (fmap largeHash128 mVal)), Ok mVal)

-- | Insert or overwrite @key@, and record the change so a comp body reading
-- it sees the update -- the source-side mutation hook used for the
-- benchmark's live-update phase (mirrors
-- 'Control.Computations.FlowImpls.HashMapFlow.hmfInsert').
sysInsert :: SystemSrc -> Key -> Val -> IO ()
sysInsert sys key val = atomically $ do
  modifyTVar' (sys_dataVar sys) (HashMap.insert key val)
  modifyTVar'
    (sys_changesVar sys)
    (HashSet.union (HashSet.singleton (Dep key (Just (largeHash128 val)))))

-- | Total number of 'compSrcExecute' calls this instance has served so far.
sysCallCount :: SystemSrc -> IO Int
sysCallCount sys = readIORef (sys_callCount sys)

-- | The largest number of 'compSrcExecute' calls against this instance ever
-- observed genuinely in flight at once (see 'executeImpl'). 1 means every
-- call so far ran strictly after the previous one finished -- i.e. no
-- overlap was ever observed, regardless of what caused that (width 1,
-- 'FlowSerial', or simply no batch wide enough to dispatch a job).
sysHighWaterMark :: SystemSrc -> IO Int
sysHighWaterMark sys = readTVarIO (sys_highWater sys)
