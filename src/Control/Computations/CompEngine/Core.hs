{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Computations.CompEngine.Core (
  -- * Garbage, i.e. output produced by compuation applications that are

  -- no longer alive.
  Garbage (..),
  emptyGarbage,
  isGarbageEmpty,

  -- * Abstract interface to the state of an implementation of the engine

  -- running computations.
  CompEngineStateIf (..),
  CapEvaluationFinished,
  CapLookup (..),
  CapCached (..),
  CapResult (..),
  EnqueueInfo (..),
  RecompInfo (..),
  fromCapResult,
  optionToCapResult,
  capResultToVer,
  capCachedHash,
  mkRecompInfoMap,
  logStale,
  dependOn,
  tellDep,

  -- * Abstract interface to the computation engine
  CompEngineIfs (..),
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Logging
import Control.Computations.Utils.MultiMap as MM
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad.Reader
import Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable

data Garbage = Garbage
  { garbage_caps :: HashSet AnyCompAp
  -- ^ Caps that were garbage collected
  , garbage_deps :: HashSet AnyCompSrcKey
  -- ^ Dependencies that are no longer referenced
  , garbage_outputs :: HashMap AnyCompAp AnyCompSinkOutsMap
  -- ^ Outputs that are no longer generated
  -- indexed by the computation that created them
  }
  deriving (Eq, Show)

instance Semigroup Garbage where
  (<>) (Garbage caps1 deps1 outs1) (Garbage caps2 deps2 outs2) =
    Garbage
      (caps1 <> caps2)
      (deps1 <> deps2)
      (HashMap.unionWith unionAnyCompSinkOutsMap outs1 outs2)

instance Monoid Garbage where
  mempty = Garbage HashSet.empty HashSet.empty mempty

emptyGarbage :: Garbage
emptyGarbage = mempty

isGarbageEmpty :: Garbage -> Bool
isGarbageEmpty (Garbage caps deps outs) =
  HashSet.null caps && HashSet.null deps && HashMap.null outs

data CompEngineStateIf m = CompEngineStateIf
  { lookupCapResult
      :: forall a
       . IsCompResult a
      => CompAp a
      -> m (CapLookup (CapResult (CapCached a)))
  , lookupCapResultDequeueIfStale
      :: forall a
       . IsCompResult a
      => Bool
      -- ^ @staleOk@ -- 'True' iff the caller is happy with a cached result
      -- even when it's stale (e.g. 'Control.Computations.CompEngine.Types.CompReqCache'),
      -- so no dequeue should happen at all.
      -> CompAp a
      -> m (CapLookup (CapResult (CapCached a)), Bool)
      -- ^ the lookup, exactly as 'lookupCapResult' would return it, paired
      -- with whether this call *also* removed @cap@ from the stale queue.
      --
      -- Fuses 'lookupCapResult' and 'dequeueGivenCap' into one round trip:
      -- the stale queue is already the authoritative dirty bit consulted on
      -- every cache lookup ('dequeueGivenCap' both asks "is this stale" and
      -- removes it in the same step), so splitting "is this cached" and "is
      -- it stale" across two separate state-if calls -- two lock
      -- acquisitions to answer one logical question -- was always
      -- artificial. See 'Control.Computations.CompEngine.Impl'\'s
      -- @evalWithCacheValue@, the only caller, for how the pair below is
      -- used.
      --
      -- The dequeue decision replicates 'dequeueGivenCap'\'s call sites in
      -- @Impl.hs@ exactly (this is the one piece of caller policy pulled
      -- into the state layer, and only as far as avoiding the second round
      -- trip requires -- everything else, e.g. what to log or whether to
      -- recompute, still lives in @Impl.hs@):
      --
      --   * a cache miss ('CapNotFound') always dequeues, regardless of
      --     @staleOk@ -- matching @withCapLookup@\'s unconditional
      --     'dequeueGivenCap' call today;
      --   * a metadata-only hit ('CapMetaCached') never dequeues -- it
      --     recomputes unconditionally either way, so there is nothing to
      --     ask the queue;
      --   * any other hit ('CapValueCached' or a cached failure) dequeues
      --     iff @staleOk@ is 'False'.
  , capEvaluationStarted :: forall a. IsCompResult a => CompAp a -> m ()
  , capEvaluationFinished :: forall a. IsCompResult a => CapEvaluationFinished m a
  , dequeueGivenCap :: forall a. IsCompResult a => CompAp a -> m Bool
  -- ^ kept standalone (not folded away once 'lookupCapResultDequeueIfStale'
  -- existed) because "Tests/TestStateIf.hs" drives it directly, as a
  -- dequeue-only operation with no accompanying lookup.
  , dequeueNextCap :: m (Maybe AnyCompAp)
  , staleQueueSize :: m Int
  , enqueueStaleCaps :: forall t. Foldable t => t CompEngDep -> m EnqueueInfo
  , getCompSinkOuts :: forall s. CompSink s => s -> m (CompSinkOuts s)
  , getQueue :: m [AnyCompAp]
  -- ^ view of the queue, just for tests
  }

type CapEvaluationFinished m a =
  CompAp a
  -> DepSet
  -> AnyCompSinkOutsMap
  -- ^ every sink output this cap's evaluation produced -- see
  -- 'Control.Computations.CompEngine.Types.CompMEnv's 'cme_outputs' haddock
  -- for why this arrives as a plain argument now instead of being read back
  -- out of shared state.
  -> Maybe a
  -> m
      ( HashSet AnyCompAp -- stale caps (only for logging purposes)
      , Garbage -- caps and tracked outputs that were garbage collected
      )

data CapLookup a
  = CapNotFound
  | CapFound a
  deriving (Show, Eq, Functor)

data CapResult a
  = CapSuccess a
  | CapFailure
  deriving (Show, Eq, Functor, Typeable)

fromCapResult :: a -> CapResult a -> a
fromCapResult def CapFailure = def
fromCapResult _ (CapSuccess x) = x

optionToCapResult :: Option a -> CapResult a
optionToCapResult None = CapFailure
optionToCapResult (Some x) = CapSuccess x

data CapCached a
  = CapMetaCached CompCacheMeta
  | CapValueCached (CompApResult a)
  deriving (Show, Eq)

capResultToVer :: CapResult (CapCached a) -> CompDepVer
capResultToVer r =
  CompDepVer $
    case r of
      CapFailure -> None
      CapSuccess x -> Some (capCachedHash x)

capCachedHash :: CapCached a -> Hash128
capCachedHash cc =
  case cc of
    CapMetaCached ccm -> ccm_largeHash ccm
    CapValueCached cv -> ccv_largeHash (cr_cacheValue cv)

data EnqueueInfo = EnqueueInfo
  { ei_affectedCaps :: Map AnyCompAp RecompInfo
  , ei_currentQueueSize :: Int
  }
  deriving (Show, Eq)

newtype RecompInfo = RecompInfo
  { ri_recompTrigger :: HashSet CompEngDep
  }
  deriving (Show, Eq)

mkRecompInfoMap
  :: (Foldable t)
  => [(CompEngDep, t AnyCompAp)]
  -> (Map AnyCompAp RecompInfo)
mkRecompInfoMap depsWithStaleCaps =
  F.foldl' insertForDep Map.empty depsWithStaleCaps
 where
  insertForDep m (dep, caps) =
    F.foldl' (insertForCap dep) m caps
  insertForCap dep m cap =
    let f _ (RecompInfo old) = RecompInfo (HashSet.insert dep old)
     in Map.insertWith f cap (RecompInfo (HashSet.singleton dep)) m

logStale :: (Foldable t, MonadIO m) => String -> t AnyCompAp -> m ()
logStale changerepr foldable =
  case caps of
    [] -> logDebug (changerepr ++ " didn't lead to stale caps.")
    [key] -> logInfo (changerepr ++ " invalidates " ++ show key)
    _
      | count < 10 ->
          do
            logInfo (changerepr ++ " invalidates the following caps:")
            mapM_ (logInfo . (" " ++) . show) caps
      | MM.numberOfKeys capsByType < 20 ->
          do
            logInfo $
              changerepr
                ++ " invalidates "
                ++ show count
                ++ " instances "
                ++ "of the following comps:"
            forM_ (MM.toSetList capsByType) $ \(ty, capsOfTy@(HashSet.size -> capsOfTyCount)) ->
              if capsOfTyCount < 5
                then mapM_ (logInfo . (" " ++) . show) capsOfTy
                else do
                  logInfo $
                    show capsOfTyCount
                      ++ " caps of type "
                      ++ show ty
                      ++ ". "
                      ++ "Here are 3 of them:"
                  mapM_ (logInfo . ("  " ++) . show) (take 3 (HashSet.toList capsOfTy))
      | otherwise ->
          do
            logInfo ("Oh! " ++ changerepr ++ " led to " ++ show count ++ " stale caps!")
            mapM_ (\k -> logDebug (show k)) caps
 where
  caps = map anyCapId $ F.toList foldable
  count = length caps
  capsByType = MM.fromList [(capId_compId x, x) | x <- caps]

dependOn :: AnyCompSrcDep -> CompM ()
dependOn = tellDep . HashSet.singleton . CompEngDepSrc

{- | Records dependencies for the cap currently being evaluated. Appends to
 the per-cap-evaluation accumulator ('CompMEnv's 'cme_deps') rather than
 returning a 'HashSet' for the caller to union in: a computation chains many
 small binds, and each one would otherwise allocate and union a fresh
 'HashSet' just to plumb its dependencies back up to its caller. Writing
 into a shared mutable accumulator instead turns each dependency record
 into an O(1) cons. The fold is strict so the accumulator's list spine is
 fully built as we go rather than left as a chain of `(++)` thunks hanging
 off the 'IORef'.
-}
tellDep :: DepSet -> CompM ()
tellDep deps
  | HashSet.null deps = compMFinished (CompResultOk ())
  | otherwise =
      CompM $ \env -> do
        modifyIORef' (cme_deps env) (\old -> HashSet.foldl' (flip (:)) old deps)
        return (CompFinished (CompResultOk ()))

data CompEngineIfs = CompEngineIfs
  { ce_compFlowRegistry :: CompFlowRegistry
  , ce_stateIf :: CompEngineStateIf IO
  }
