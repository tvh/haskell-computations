{-# LANGUAGE TupleSections #-}

{- |
Module      : Mgw.Util.MultiMap
Description : Map keys to multiple values.  Each key value pair is contained at most once.
-}
module Control.Computations.Utils.MultiMap (
  MultiMap (..),
  delete,
  elems,
  empty,
  filter,
  filterWithKey,
  fromList,
  toList,
  insert,
  keys,
  lookup,
  union,
  numberOfKeys,
  numberOfKeyValuePairs,
  toSetList,
  fromSetList,
)
where

----------------------------------------
-- EXTERNAL
----------------------------------------
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.List as L
import Test.QuickCheck (Arbitrary (..))
import Prelude hiding (filter, lookup)

newtype MultiMap k v = MultiMap (HashMap k (HashSet v))
  deriving (Eq)

instance
  (Hashable k, Hashable v, Arbitrary k, Arbitrary v)
  => Arbitrary (MultiMap k v)
  where
  arbitrary = fromList <$> arbitrary

instance (Show k, Show v) => Show (MultiMap k v) where
  show x = show $ "MultiMap.fromList " ++ show (toList x)

instance (Hashable k, Hashable v) => Semigroup (MultiMap k v) where
  (<>) = union

instance (Hashable k, Hashable v) => Monoid (MultiMap k v) where
  mempty = empty

numberOfKeys :: MultiMap k v -> Int
numberOfKeys (MultiMap m) = HashMap.size m

numberOfKeyValuePairs :: MultiMap k v -> Int
numberOfKeyValuePairs (MultiMap m) = F.foldl' (\acc el -> acc + HashSet.size el) 0 m

fromList :: (Hashable k, Hashable v) => [(k, v)] -> MultiMap k v
fromList = L.foldl' (\m (k, v) -> insert k v m) empty

toList :: MultiMap k v -> [(k, v)]
toList (MultiMap hm) = concatMap (\(k, v) -> map (k,) (F.toList v)) $ HashMap.toList hm

toSetList :: MultiMap k v -> [(k, HashSet v)]
toSetList (MultiMap hm) = HashMap.toList hm

fromSetList :: Hashable k => [(k, HashSet v)] -> MultiMap k v
fromSetList setList = MultiMap (HashMap.fromList setList)

empty :: MultiMap k v
empty = MultiMap HashMap.empty

insert :: (Hashable k, Hashable v) => k -> v -> MultiMap k v -> MultiMap k v
insert k v (MultiMap hm) = MultiMap $! HashMap.insert k (HashSet.insert v oldSet) hm
 where
  oldSet = HashMap.findWithDefault HashSet.empty k hm

lookup :: (Hashable k) => k -> MultiMap k v -> HashSet v
lookup k (MultiMap hm) = HashMap.findWithDefault HashSet.empty k hm

delete :: (Hashable k, Hashable v) => k -> v -> MultiMap k v -> MultiMap k v
delete k v (MultiMap hm) = MultiMap $! HashMap.adjust (HashSet.delete v) k hm

keys :: MultiMap k v -> [k]
keys (MultiMap hm) = HashMap.keys hm

-- | Returns all elements of the multimap in an undefined order.
elems :: MultiMap k v -> [v]
elems (MultiMap hm) = concatMap HashSet.toList $ HashMap.elems hm

union :: (Hashable k, Hashable v) => MultiMap k v -> MultiMap k v -> MultiMap k v
union (MultiMap left) (MultiMap right) =
  MultiMap $
    HashMap.unionWith HashSet.union left right

filterWithKey
  :: forall k v
   . (Hashable k)
  => (k -> v -> Bool)
  -> MultiMap k v
  -> MultiMap k v
filterWithKey f (MultiMap m) =
  MultiMap (HashMap.foldlWithKey go HashMap.empty m)
 where
  go :: HashMap k (HashSet v) -> k -> HashSet v -> HashMap k (HashSet v)
  go old k oldSet =
    let newSet = HashSet.filter (f k) oldSet
     in if HashSet.null newSet
          then old
          else HashMap.insert k newSet old

filter
  :: forall k v
   . (Hashable k)
  => (v -> Bool)
  -> MultiMap k v
  -> MultiMap k v
filter f = filterWithKey (\_ v -> f v)

