{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | The cache of computation results, keyed by the caller-interned 'Int' id
 of a cap (see "Control.Computations.CompEngine.Utils.Intern") rather than
 by 'AnyCompAp' directly. This used to be a @Data.Map AnyCompAp@, whose
 'Ord' instance dispatches through 'eqT' on every comparison in every
 O(log n) tree descent -- profiling identified this as one of the two
 dominant costs in the engine (see docs/benchmark-notes.md, Stage 0.5's
 profile). Keying by 'Int' instead turns every lookup/insert/delete into a
 dense 'IntMap' operation with no 'eqT'/'Ord' dispatch at all.

 Each entry still needs to know which 'CompId' it belongs to (for the
 per-computation size bookkeeping in 'sifc_compToSize'), so 'insert' takes
 the 'AnyCompAp' alongside its id purely to derive that -- and the derived
 'CompId' is cached per id in 'sifc_keyToCompId' so 'delete' only ever
 needs the id.
-}
module Control.Computations.CompEngine.SifCache (
  SifCache,
  CompSize (..),
  empty,
  null,
  insert,
  delete,
  lookup,
  size,
  keysSet,
  totalInstanceCount,
  dataSizeForCompId,
  compIdSizeMap,
  compIdSizeList,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.DataSize
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad.Identity
import Data.Int
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Framework
import Prelude hiding (lookup, null)

data AbstractSifCache a = AbstractSifCache
  { sifc_keyToVal :: IntMap a
  , sifc_keyToCompId :: IntMap CompId
  -- ^ the 'CompId' each entry belongs to, cached at insertion time so
  -- 'delete' doesn't need to be handed an 'AnyCompAp' just to re-derive it.
  , sifc_compToSize :: Map CompId CompSize
  }
  deriving (Show, Eq)

data CompSize = CompSize
  { cs_dataSize :: Option DataSize
  , cs_size :: Option Int
  , cs_instanceCount :: Int
  }
  deriving (Show, Eq)

instance Semigroup CompSize where
  (<>) = csPlus

instance Monoid CompSize where
  mempty = CompSize (Some 0) (Some 0) 0

csPlus :: CompSize -> CompSize -> CompSize
csPlus (CompSize mds1 ms1 c1) (CompSize mds2 ms2 c2) =
  let cs_dataSize =
        case (mds1, mds2) of
          (Some ds1, Some ds2) -> Some (ds1 + ds2)
          _ -> None
      cs_instanceCount = c1 + c2
      cs_size =
        case (ms1, ms2) of
          (Some s1, Some s2) -> Some (s1 + s2)
          _ -> None
   in CompSize{..}

csMinus :: CompSize -> CompSize -> CompSize
csMinus (CompSize mds1 ms1 c1) (CompSize mds2 ms2 c2) =
  let cs_dataSize =
        case (mds1, mds2) of
          (Some ds1, Some ds2) -> Some (ds1 - ds2)
          _ -> None
      cs_instanceCount = c1 - c2
      cs_size =
        case (ms1, ms2) of
          (Some s1, Some s2) -> Some (s1 - s2)
          _ -> None
   in CompSize{..}

compSize :: HasSizes a => a -> CompSize
compSize x =
  CompSize
    { cs_dataSize = dataSize x
    , cs_size = intSize x
    , cs_instanceCount = 1
    }

type SifCache = AbstractSifCache (CapResult AnyCompCacheValue)

class HasSizes a where
  dataSize :: a -> Option DataSize
  intSize :: a -> Option Int

instance HasSizes (CapResult AnyCompCacheValue) where
  dataSize = fromCapResult (Some 0) . fmap (anyCompCacheValueApply ccv_cacheSize)
  intSize = fromCapResult None . fmap (anyCompCacheValueApply $ ccm_cachedSize . ccv_meta)

empty :: AbstractSifCache a
empty = AbstractSifCache IntMap.empty IntMap.empty Map.empty

size :: AbstractSifCache a -> Int
size = IntMap.size . sifc_keyToVal

null :: AbstractSifCache a -> Bool
null = IntMap.null . sifc_keyToVal

-- | The set of ids currently present in the cache.
keysSet :: AbstractSifCache a -> IntSet
keysSet = IntMap.keysSet . sifc_keyToVal

insert :: (HasSizes a) => Int -> AnyCompAp -> a -> AbstractSifCache a -> AbstractSifCache a
insert keyId (AnyCompAp cap) val cache@(AbstractSifCache keyToVal keyToCompId compToSize) =
  AbstractSifCache
    { sifc_keyToVal = IntMap.insert keyId val keyToVal
    , sifc_keyToCompId = IntMap.insert keyId compId keyToCompId
    , sifc_compToSize = Map.insert compId newCompSize compToSize
    }
 where
  newCompSize = oldCompIdSize `csPlus` newEntrySize `csMinus` oldEntrySize
  compId = capCompId cap
  oldCompIdSize = dataSizeForCompId compId cache
  oldEntrySize = maybe mempty compSize $ IntMap.lookup keyId keyToVal
  newEntrySize = compSize val

delete :: HasSizes a => Int -> AbstractSifCache a -> AbstractSifCache a
delete keyId cache@(AbstractSifCache keyToVal keyToCompId compToSize) =
  case IntMap.lookup keyId keyToCompId of
    Nothing -> cache
    Just compId ->
      AbstractSifCache
        { sifc_keyToVal = IntMap.delete keyId keyToVal
        , sifc_keyToCompId = IntMap.delete keyId keyToCompId
        , sifc_compToSize =
            if oldCompIdSize == oldEntrySize
              then Map.delete compId compToSize
              else Map.insert compId (oldCompIdSize `csMinus` oldEntrySize) compToSize
        }
     where
      oldCompIdSize = dataSizeForCompId compId cache
      oldEntrySize = maybe mempty compSize $ IntMap.lookup keyId keyToVal

lookup :: Int -> AbstractSifCache a -> Maybe a
lookup keyId cache = IntMap.lookup keyId (sifc_keyToVal cache)

compIdSizeMap :: AbstractSifCache a -> Map CompId CompSize
compIdSizeMap = sifc_compToSize

compIdSizeList :: AbstractSifCache a -> [(CompId, CompSize)]
compIdSizeList = Map.toList . compIdSizeMap

totalInstanceCount :: AbstractSifCache a -> Int
totalInstanceCount = foldl' (+) 0 . map (cs_instanceCount . snd) . Map.toList . sifc_compToSize

dataSizeForCompId :: CompId -> AbstractSifCache a -> CompSize
dataSizeForCompId compId = Map.findWithDefault mempty compId . sifc_compToSize

instance HasSizes Int where
  dataSize = Some . bytes
  intSize = Some

fromList :: HasSizes a => [(AnyCompAp, a)] -> AbstractSifCache a
fromList = snd . foldl' step (0 :: Int, empty)
 where
  step (!nextId, !cache) (key, val) = (nextId + 1, insert nextId key val cache)

test_fromList :: IO ()
test_fromList = assertEqual actual desired
 where
  keyToVal =
    [ (mkAnyCompAp "c1" "a", 1 :: Int)
    , (mkAnyCompAp "c1" "b", 2)
    , (mkAnyCompAp "c2" "c", 5)
    ]
  actual = fromList keyToVal
  desired =
    AbstractSifCache
      (IntMap.fromList [(0, 1), (1, 2), (2, 5)])
      (IntMap.fromList [(0, "c1"), (1, "c1"), (2, "c2")])
      ( Map.fromList
          [ ("c1", CompSize (Some $ DataSize 3) (Some 3) 2)
          , ("c2", CompSize (Some $ DataSize 5) (Some 5) 1)
          ]
      )

test_insert :: IO ()
test_insert = assertEqual actual desired
 where
  keyValuePairs =
    [ (mkAnyCompAp "c1" "a", 1 :: Int)
    , (mkAnyCompAp "c1" "b", 2)
    , (mkAnyCompAp "c2" "c", 5)
    ]
  actual = snd $ foldl' (\(!i, !c) (key, val) -> (i + 1, insert i key val c)) (0 :: Int, empty) keyValuePairs
  desired =
    AbstractSifCache
      (IntMap.fromList [(0, 1), (1, 2), (2, 5)])
      (IntMap.fromList [(0, "c1"), (1, "c1"), (2, "c2")])
      ( Map.fromList
          [ ("c1", CompSize (Some $ DataSize 3) (Some 3) 2)
          , ("c2", CompSize (Some $ DataSize 5) (Some 5) 1)
          ]
      )

test_delete :: IO ()
test_delete = assertEqual actual desired
 where
  initial =
    fromList
      [ (mkAnyCompAp "c1" "a", 1 :: Int) -- id 0
      , (mkAnyCompAp "c1" "b", 2) -- id 1
      , (mkAnyCompAp "c2" "c", 5) -- id 2
      , (mkAnyCompAp "c3" "c", 11) -- id 3
      ]
  actual = foldl' (flip delete) initial [1, 3]
  desired =
    AbstractSifCache
      (IntMap.fromList [(0, 1), (2, 5)])
      (IntMap.fromList [(0, "c1"), (2, "c2")])
      ( Map.fromList
          [ ("c1", CompSize (Some $ DataSize 1) (Some 1) 1)
          , ("c2", CompSize (Some $ DataSize 5) (Some 5) 1)
          ]
      )

test_deleteIsIdempotent :: IO ()
test_deleteIsIdempotent =
  do
    let initial = fromList [(mkAnyCompAp "c1" "a", 1 :: Int)]
        onceDeleted = delete 0 initial
    assertEqual onceDeleted (delete 0 onceDeleted)

unitCaching :: CompCacheBehavior ()
unitCaching =
  CompCacheBehavior
    { ccb_memcache = \() ->
        CompCacheValue
          { ccv_payload = Some ()
          , ccv_meta =
              CompCacheMeta
                { ccm_largeHash = largeHash128 ()
                , ccm_logrepr = "()"
                , ccm_approxCachedSize = Some (bytes (16 :: Int))
                , ccm_cachedSize = Some 1
                }
          }
    }

mkAnyCompAp :: String -> String -> AnyCompAp
mkAnyCompAp compName param =
  AnyCompAp (mkCompAp comp param)
 where
  comp :: Comp String ()
  comp = Comp (mkCompId compName) unitCaching (\_ -> return ()) mempty
