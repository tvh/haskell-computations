{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | A simple bidirectional intern table: hands out a dense 'Int' id for every
 distinct key seen (get-or-create, via 'intern'), and can resolve an id back
 to its key.

 This is the core mechanism behind the state layer's interning: instead of
 threading the (comparatively expensive to hash/compare -- see 'AnyCompAp's
 'Eq'/'Ord', which dispatch through 'eqT') key type through every
 persistent container, the state layer interns it once and threads a plain
 'Int' everywhere else. An 'Int' id is also its own canonical identity: two
 calls to 'intern' with different, but structurally equal, keys always
 return the same id. This replaces the old "lookupLE then check for the
 canonical shared object" trick that 'SifCache.lookup' and
 'SimpleStateIf.normalizeDep' used to perform for the same purpose.

 Ids are never recycled (see 'release') -- 'IntMap'/'IntSet' handle sparse
 keys fine, and reusing an id would risk a stale entry left behind in some
 container transiently still keying off the old id.
-}
module Control.Computations.CompEngine.Utils.Intern (
  InternTable,
  empty,
  intern,
  lookupId,
  resolve,
  release,
  size,
  liveIds,
  htf_thisModulesTests,
)
where

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.List (foldl')
import Test.Framework
import Prelude hiding (lookup)

data InternTable k = InternTable
  { it_fwd :: !(HashMap k Int)
  , it_rev :: !(IntMap k)
  , it_next :: !Int
  }
  deriving (Show)

empty :: InternTable k
empty = InternTable HashMap.empty IntMap.empty 0

-- | Get-or-create the interned id for a key.
intern :: (Hashable k) => k -> InternTable k -> (Int, InternTable k)
intern k t@(InternTable fwd rev nextId) =
  case HashMap.lookup k fwd of
    Just !i -> (i, t)
    Nothing ->
      let !i = nextId
          !fwd' = HashMap.insert k i fwd
          !rev' = IntMap.insert i k rev
       in (i, InternTable fwd' rev' (nextId + 1))

-- | Look up the id of a key without interning it. 'Nothing' means the key
-- has never been interned (and, in particular, nothing in the state layer
-- can currently be pointing at it).
lookupId :: (Hashable k) => k -> InternTable k -> Maybe Int
lookupId k t = HashMap.lookup k (it_fwd t)

{- | Resolve an id back to its key. Errors out if the id was never interned
 or has since been 'release'd -- every id flowing through the state layer
 must originate from a prior 'intern' call and only be resolved before it
 is released, so hitting this indicates a lifecycle bug in the caller.
-}
resolve :: Int -> InternTable k -> k
resolve i t =
  case IntMap.lookup i (it_rev t) of
    Just k -> k
    Nothing ->
      error
        ( "Intern.resolve: id "
            ++ show i
            ++ " is not (or no longer) interned"
        )

-- | Release an id, e.g. once the corresponding entry has been garbage
-- collected from every container that keys off it. Removes the mapping in
-- both directions; does nothing if the id is already gone.
release :: (Hashable k) => Int -> InternTable k -> InternTable k
release i t =
  case IntMap.lookup i (it_rev t) of
    Nothing -> t
    Just k ->
      t
        { it_fwd = HashMap.delete k (it_fwd t)
        , it_rev = IntMap.delete i (it_rev t)
        }

-- | Number of currently-live (interned, not yet released) ids.
size :: InternTable k -> Int
size = IntMap.size . it_rev

-- | The set of currently-live ids.
liveIds :: InternTable k -> IntSet
liveIds = IntMap.keysSet . it_rev

--
-- Tests
--

test_internIsIdempotent :: IO ()
test_internIsIdempotent =
  do
    let (i1, t1) = intern "a" (empty :: InternTable String)
        (i2, t2) = intern "a" t1
    assertEqual i1 i2
    assertEqual 1 (size t2)

test_internDistinctKeysGetDistinctIds :: IO ()
test_internDistinctKeysGetDistinctIds =
  do
    let (i1, t1) = intern "a" (empty :: InternTable String)
        (i2, t2) = intern "b" t1
    assertBool (i1 /= i2)
    assertEqual 2 (size t2)

test_resolveRoundTrips :: IO ()
test_resolveRoundTrips =
  do
    let (i, t) = intern "hello" (empty :: InternTable String)
    assertEqual "hello" (resolve i t)

test_releaseThenReinternGetsFreshId :: IO ()
test_releaseThenReinternGetsFreshId =
  do
    let (i1, t1) = intern "a" (empty :: InternTable String)
        t2 = release i1 t1
        (i2, t3) = intern "a" t2
    assertBool (i1 /= i2)
    assertEqual 1 (size t3)
    assertEqual Nothing (lookupId "a" t2)

test_manyInternsAreAllResolvable :: IO ()
test_manyInternsAreAllResolvable =
  do
    let keys = map show [1 .. (200 :: Int)]
        t = Data.List.foldl' (\acc k -> snd (intern k acc)) (empty :: InternTable String) keys
    assertEqual (length keys) (size t)
    mapM_ (\k -> assertEqual k (resolve (lookupIdOrErr k t) t)) keys
 where
  lookupIdOrErr k t =
    case lookupId k t of
      Just i -> i
      Nothing -> error "expected key to be interned"
