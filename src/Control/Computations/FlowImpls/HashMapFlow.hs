{-# LANGUAGE TypeFamilies #-}

{- | An in-memory key\/value store that is both a
 'Control.Computations.CompEngine.CompSrc.CompSrc' and a
 'Control.Computations.CompEngine.CompSink.CompSink' -- a single
 'HashMapFlow' handle can be registered as both, giving comp bodies
 somewhere to read from and write to without a filesystem or database
 anywhere in sight.

 That combination makes it the natural tool for:

 * writing tests of your own computations, with no real I\/O backing them;
 * learning\/trying the engine's API without standing up a real source or
   sink;
 * isolating engine behaviour from I\/O in a benchmark or experiment --
   which is exactly why this repo's own benchmark uses it.

 To be clear about what it is not: it is in-memory only, so nothing
 survives process exit and there is no persistence layer to configure.
 Versions ('Ver') are content hashes
 ('Control.Computations.Utils.Hash.Hash128') of the stored value, recomputed
 on every write -- there is no separate version counter.

 A minimal usage sketch:

 > flow <- initHashMapFlow "my-flow"
 > registerCompSrc  reg flow  -- comp bodies can now read through it
 > registerCompSink reg flow  -- comp bodies can now write through it
 > hmfInsert flow "key" "value"   -- seed or mutate state from outside a comp
 > m <- getHashMap flow           -- assert on the whole map in a test
-}
module Control.Computations.FlowImpls.HashMapFlow (
  HashMapFlow,
  HashMapReadReq (..),
  HashMapWriteReq (..),
  initHashMapFlow,
  Key,
  Val,
  Ver,

  -- * Primary interface
  hmfLookup,
  hmfInsert,
  getHashMap,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Logging
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Data.Typeable

type Key = BS.ByteString
type Val = BS.ByteString
type Ver = Hash128

type HashMapDep = Dep Key (Maybe Ver)

data HashMapFlow = HashMapFlow
  { hmf_ident :: T.Text
  , hmf_hashMapVar :: TVar (HashMap Key Val)
  , hmf_changesVar :: TVar (HashSet HashMapDep)
  }
  deriving (Typeable)

data HashMapReadReq a where
  HashMapLookupReq :: Key -> HashMapReadReq (Maybe Val)

instance CompSrc HashMapFlow where
  type CompSrcReq HashMapFlow = HashMapReadReq
  type CompSrcKey HashMapFlow = Key
  type CompSrcVer HashMapFlow = Maybe Ver
  compSrcInstanceId = CompSrcInstanceId . hmf_ident
  compSrcExecute = executeImpl
  compSrcUnregister = unregisterImpl
  compSrcWaitChanges = waitChangesImpl

  -- 'executeImpl' is a 'readTVarIO' followed by a pure 'HashMap.lookup'.
  compSrcConcurrency _ = FlowConcurrent

initHashMapFlow :: T.Text -> IO HashMapFlow
initHashMapFlow ident = do
  hmV <- newTVarIO HashMap.empty
  setV <- newTVarIO HashSet.empty
  pure (HashMapFlow ident hmV setV)

waitChangesImpl :: HashMapFlow -> STM (HashSet HashMapDep)
waitChangesImpl hmf = do
  set <- readTVar (hmf_changesVar hmf)
  writeTVar (hmf_changesVar hmf) HashSet.empty
  if HashSet.null set
    then do
      logDebugSTM ("no changes in HashMapFlow " ++ T.unpack (hmf_ident hmf))
      retry
    else do
      logDebugSTM
        ( show (HashSet.size set)
            ++ " changes in HashMapFlow "
            ++ T.unpack (hmf_ident hmf)
        )
      pure set

executeImpl :: HashMapFlow -> HashMapReadReq a -> IO (HashSet HashMapDep, Fail a)
executeImpl hmf (HashMapLookupReq key) =
  do
    mVal <- hmfLookup hmf key
    return (HashSet.singleton (Dep key (fmap largeHash128 mVal)), Ok mVal)

{- | Look up the value stored at @key@, if any. Reflects whatever the most
 recent 'hmfInsert' (or a comp body's write through the
 'Control.Computations.CompEngine.CompSink.CompSink' instance) last wrote --
 there is no on-disk state to fall back on.
-}
hmfLookup :: HashMapFlow -> Key -> IO (Maybe Val)
hmfLookup hmf key = do
  m <- readTVarIO (hmf_hashMapVar hmf)
  pure (HashMap.lookup key m)

unregisterImpl :: HashMapFlow -> HashSet Key -> IO ()
unregisterImpl _ _ = pure ()

data HashMapWriteReq a where
  HashMapStoreReq :: Key -> Val -> HashMapWriteReq ()

instance CompSink HashMapFlow where
  type CompSinkReq HashMapFlow = HashMapWriteReq
  type CompSinkOut HashMapFlow = Key
  compSinkInstanceId = CompSinkInstanceId . hmf_ident
  compSinkExecute = executeWriteImpl
  compSinkDeleteOutputs = deleteOutputsImpl

  -- not all sinks support listing the existing outputs
  compSinkListExistingOutputs _ = None

executeWriteImpl :: HashMapFlow -> HashMapWriteReq a -> IO (HashSet Key, Fail a)
executeWriteImpl hmf (HashMapStoreReq k v) = do
  hmfInsert hmf k v
  return (HashSet.singleton k, Ok ())

{- | Insert or overwrite the value at @key@, and record the change so any
 comp body reading @key@ through the
 'Control.Computations.CompEngine.CompSrc.CompSrc' instance sees it. This is
 the primary way to seed or mutate a 'HashMapFlow'\'s state from outside a
 comp body -- e.g. from test setup.
-}
hmfInsert :: HashMapFlow -> Key -> Val -> IO ()
hmfInsert hmf key val = do
  logDebug ("HashMapFlow: Inserting " ++ show key ++ " -> " ++ show val)
  atomically $ do
    modifyTVar' (hmf_hashMapVar hmf) (HashMap.insert key val)
    modifyTVar'
      (hmf_changesVar hmf)
      (HashSet.union (HashSet.singleton (Dep key (Just (largeHash128 val)))))

deleteOutputsImpl :: HashMapFlow -> HashSet Key -> IO ()
deleteOutputsImpl hmf keys = do
  logDebug ("HashMapFlow: Deleting " ++ show keys)
  atomically (modifyTVar' (hmf_hashMapVar hmf) $ \hm -> F.foldl' (flip HashMap.delete) hm keys)

{- | Snapshot the entire underlying map. Useful in tests to assert on the
 whole set of key\/value pairs a computation has written, rather than
 looking up one key at a time.
-}
getHashMap :: HashMapFlow -> IO (HashMap Key Val)
getHashMap hmdi =
  readTVarIO (hmf_hashMapVar hmdi)
