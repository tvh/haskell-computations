{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Computations.CompEngine.Utils.OutputsMap (
  -- * Map type
  OutputsMap,
  OutputsForwardMap,
  OutputsReverseMap,
  OutputKeyHash,

  -- * Query
  forwardMap,
  lookup,
  lookupOutputKey,

  -- * Construction
  empty,
  fromForwardMap,
  fromOrderedForwardMap,
  insert,
  insertWith,
  delete,

  -- * Other
  filterUnreferencedOutputs,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlow
import Control.Computations.CompEngine.CompSink

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Typeable
import qualified StrictList as SL
import Prelude hiding (lookup)

-- This is a map connecting keys to outputs, which can be queried in both directions.
-- The 'om_forward' map provides a direct way to lookup all outputs for a key.
-- For the other direction, we use the hashes of outputs. The map under 'om_reverse' stores for
-- every hash of an output which keys have an (there may be multiple!) output with that hash value.
-- When querying for an output, we have to also do a lookup in the 'om_forward' map to filter out
-- keys that only have another output which by coincidence has the same hash value.

type OutputsForwardMap k = HashMap k AnyCompSinkOutsMap
type OutputsReverseMap k =
  IntMap (SL.List k)
  -- ^ the strict list must be in ascending order

type OutputKeyHash = Int

-- | Both fields explicitly strict, though (see "Utils/PriorityAgingQueue.hs"
-- for the same correction) this project's @-XStrictData@ default
-- (package.yaml) already made them strict without the explicit @!@ -- kept
-- as documentation\/robustness rather than because it changed observed
-- behavior. This map is rebuilt (via 'insert'\/'delete'\/'insertWith') on
-- essentially every cap finish in "SimpleStateIf.hs"
-- (@commitPendingOutputsForKey@\/@freeRowCascade@), whose own write site is a
-- bare 'Data.IORef.writeIORef' -- which gives zero forcing *of the IORef's
-- new contents* regardless of the referenced value's own field strictness
-- (see 'colWrite''s haddock in "Utils/DefTable.hs"); the write site now
-- forces explicitly with @$!@, and it's that forcing -- not the field
-- annotations here -- that stops a chain of unevaluated @insert@\/@delete@
-- thunks from building up across rows.
data OutputsMap k = OutputsMap
  { om_forward :: !(OutputsForwardMap k)
  , om_reverse :: !(OutputsReverseMap k)
  }
  deriving (Show)

forwardMap :: OutputsMap k -> OutputsForwardMap k
forwardMap = om_forward

lookup
  :: (Hashable k)
  => k
  -> OutputsMap k
  -> Maybe AnyCompSinkOutsMap
lookup key om =
  HashMap.lookup key (om_forward om)

-- | Look up the output represented by the triple (reverse lookup)
lookupOutputKey
  :: (CompSink s, Hashable k)
  => (Proxy s, CompSinkId, CompSinkOut s)
  -> OutputsMap k
  -> SL.List k
lookupOutputKey outputIdent@(_proxy, _dataIfKey, outputKey) om =
  SL.filter (hasOutput outputIdent om) $
    fromMaybe mempty $
      IntMap.lookup h (om_reverse om)
 where
  h = hash outputKey

empty :: OutputsMap k
empty =
  OutputsMap
    { om_forward = HashMap.empty
    , om_reverse = IntMap.empty
    }

mkSingletonReverseMap :: k -> AnyCompSinkOutsMap -> OutputsReverseMap k
mkSingletonReverseMap key (AnyCompSinkOutsMap outsMap) =
  IntMap.fromList $
    map (\hash -> (hash, SL.Cons key SL.Nil)) $
      Map.elems outsMap >>= hashAnyOutputs

outputKeyHash :: CompSink s => Proxy s -> CompSinkOut s -> OutputKeyHash
outputKeyHash _ = hash

-- | Merges two ascending 'SL.List's into one ascending list, dropping
-- neither side's duplicates. Used to combine the per-hash key lists in
-- 'om_reverse', which the 'OutputsReverseMap' comment above requires to
-- stay in ascending order (formerly "Utils/StrictList.hs"'s @merge@\/
-- @mergeBy@, moved here as this is the only remaining call site).
merge :: Ord a => SL.List a -> SL.List a -> SL.List a
merge = mergeBy compare

mergeBy :: (a -> a -> Ordering) -> SL.List a -> SL.List a -> SL.List a
mergeBy cmp = go
 where
  go as@(SL.Cons a as') bs@(SL.Cons b bs') =
    case cmp a b of
      LT -> SL.Cons a (go as' bs)
      GT -> SL.Cons b (go as bs')
      EQ -> SL.Cons a (go as' bs')
  go SL.Nil bs = bs
  go as SL.Nil = as

hashAnyOutputs :: AnyCompSinkOuts -> [OutputKeyHash]
hashAnyOutputs (ForAnyCompFlow _ p outputsWithIdent) =
  map (outputKeyHash p) $ HashSet.toList $ unSomeCompSinkOut outputsWithIdent

-- | Helper function. Checks if the key has the output identified by the tripel.
hasOutput
  :: forall s k
   . (CompSink s, Hashable k)
  => (Proxy s, CompSinkId, CompSinkOut s)
  -> OutputsMap k
  -> k
  -> Bool
hasOutput (_, compSinkId, outputKey) om key =
  Just True
    == do
      AnyCompSinkOutsMap manyOutputs <- HashMap.lookup key (om_forward om)
      ForAnyCompFlow _ _ (SomeCompSinkOuts outputsForCompSink) <- Map.lookup compSinkId manyOutputs
      outputsForCompSink' <- gcast outputsForCompSink
      pure (HashSet.member outputKey (outputsForCompSink' :: CompSinkOuts s))

-- | Helper function. Checks if the key has an output with the given hash.
hasOutputByHash :: (Hashable k) => OutputsMap k -> OutputKeyHash -> k -> Bool
hasOutputByHash om h key =
  case HashMap.lookup key (om_forward om) of
    Nothing -> False
    Just outputs -> anyOutsMap (outputHasHash h) outputs
 where
  outputHasHash :: CompSink s => OutputKeyHash -> Proxy s -> CompSinkOut s -> Bool
  outputHasHash h _ output = hash output == h

fromForwardMapGeneric
  :: (Ord k, Hashable k)
  => (map k AnyCompSinkOutsMap -> [(k, AnyCompSinkOutsMap)])
  -> map k AnyCompSinkOutsMap
  -> OutputsMap k
fromForwardMapGeneric toKVPairs m =
  let pairs = toKVPairs m
   in OutputsMap
        { om_forward = HashMap.fromList pairs
        , om_reverse = IntMap.unionsWith merge $ map (uncurry mkSingletonReverseMap) pairs
        }

fromForwardMap
  :: (Ord k, Hashable k)
  => OutputsForwardMap k
  -> OutputsMap k
fromForwardMap = fromForwardMapGeneric HashMap.toList

fromOrderedForwardMap
  :: (Ord k, Hashable k)
  => Map k AnyCompSinkOutsMap
  -> OutputsMap k
fromOrderedForwardMap = fromForwardMapGeneric Map.toList

{- | Internally used helper function. The given outputs are not "dirty", because their entry in the
 'om_reverse' map may not be up to date (specifically, the list of keys stored there may contain
 false positives). This function brings the map up to date.
-}
cleanReverse
  :: forall k
   . (Hashable k)
  => AnyCompSinkOutsMap
  -> OutputsMap k
  -> OutputsMap k
cleanReverse changedOutputs om =
  let hashes = mapAnyOutsMap outputKeyHash changedOutputs
   in om{om_reverse = foldl' (flip cleanReverseForHash) (om_reverse om) hashes}
 where
  cleanReverseForHash :: OutputKeyHash -> IntMap.IntMap (SL.List k) -> IntMap.IntMap (SL.List k)
  cleanReverseForHash h im =
    IntMap.adjust (SL.filter (hasOutputByHash om h)) h im

insertWith
  :: (Hashable k, Ord k)
  => (AnyCompSinkOutsMap -> AnyCompSinkOutsMap -> AnyCompSinkOutsMap)
  -> k
  -> AnyCompSinkOutsMap
  -> OutputsMap k
  -> OutputsMap k
insertWith combine key outputs om =
  let mOldOutputs = HashMap.lookup key (om_forward om)
      currentOutputs = maybe outputs (combine outputs) mOldOutputs
      deletedOutputs = maybe mempty (`diffAnyOutsMap` currentOutputs) mOldOutputs
   in cleanReverse deletedOutputs $
        OutputsMap
          { om_forward = HashMap.insert key currentOutputs (om_forward om)
          , om_reverse = IntMap.unionWith merge (mkSingletonReverseMap key outputs) (om_reverse om)
          }

insert
  :: (Hashable k, Ord k)
  => k
  -> AnyCompSinkOutsMap
  -> OutputsMap k
  -> OutputsMap k
insert = insertWith (\newOutputs _oldOutputs -> newOutputs)

delete
  :: (Hashable k)
  => k
  -> OutputsMap k
  -> OutputsMap k
delete key om =
  OutputsMap
    { om_forward = HashMap.delete key (om_forward om)
    , om_reverse =
        let mOldOutputs = HashMap.lookup key (om_forward om)
            hashes = maybe [] (mapAnyOutsMap outputKeyHash) mOldOutputs
         in foldl' (\revMap h -> IntMap.adjust (SL.filter (/= key)) h revMap) (om_reverse om) hashes
    }

-- | Returns only the outputs which are *not* referenced (any more).
filterUnreferencedOutputs :: (Hashable k) => OutputsMap k -> AnyCompSinkOutsMap -> AnyCompSinkOutsMap
filterUnreferencedOutputs om = filterAnyOutsMap notReferenced
 where
  notReferenced :: CompSink s => Proxy s -> CompSinkId -> CompSinkOut s -> Bool
  notReferenced p k output =
    null (lookupOutputKey (p, k, output) om)

