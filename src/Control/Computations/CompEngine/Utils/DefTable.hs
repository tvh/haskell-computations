{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Per-definition columnar row storage: the "Tier 2" memory redesign from
 docs/benchmark-notes.md's "Memory roadmap" section, modeled on the Rust
 port's Stage 5 (@rust-computations/docs/persistence-benchmark-notes.md@,
 "Stage 5 -- Tier 2 columnar per-def tables").

 = Row identity

 A row is identified by a def index (which definition it belongs to) and a
 row number within that definition's table. Callers of this module pack the
 two into a single 'Int' via 'DefRef' rather than using a pair, so the rest
 of the state layer (the stale-cap priority queue, the outputs side table,
 anything hashing/comparing an identity) keeps working with plain 'Int's --
 the same "hot-path keys are Ints" property Stage 1's interning established,
 just reinterpreted: what used to be an arbitrary dense counter is now
 @(defIndex \<\< rowBits) .|. row@.

 = This increment

 This is the row-identity/lifecycle skeleton only: a table's per-row state
 is, for now, entirely "is this row currently allocated" -- tracked by
 whether its param hash is present in 'dt_index' and, dually, whether its
 row number is on the free list. No data columns (hash/flags/param/value/
 edges) exist yet; those arrive in later increments of the columnar
 rewrite. A row's identity is looked up/created by an already-computed
 'Hash128' (the param hash callers derive the same way the old intern table
 did) and freed by the caller supplying that same hash back (mirroring
 Stage 1's intern table, which also required the key on release since there
 was no separate reverse index for it).
-}
module Control.Computations.CompEngine.Utils.DefTable (
  -- * Row identity
  DefRef,
  packRef,
  unpackRef,
  refDefIdx,
  refRow,

  -- * The table
  DefTable,
  new,
  rowCount,

  -- * Row lifecycle
  lookupOrInsertRow,
  freeRow,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Hash (Hash128, largeHash128)

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Bits
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Test.Framework hiding ((.&.))

--
-- Row identity: DefRef = packed (defIndex, row)
--

-- | A packed @(defIndex, row)@ identity. 20 bits of def index (~1M
-- definitions), 44 bits of row (~17 trillion rows per definition) -- both
-- comically generous versus this engine's actual scale, chosen to leave
-- headroom rather than to be tight.
type DefRef = Int

rowBits :: Int
rowBits = 44

rowMask :: Int
rowMask = (1 `shiftL` rowBits) - 1

packRef :: Int -> Int -> DefRef
packRef defIdx row = (defIdx `shiftL` rowBits) .|. (row .&. rowMask)
{-# INLINE packRef #-}

unpackRef :: DefRef -> (Int, Int)
unpackRef ref = (ref `shiftR` rowBits, ref .&. rowMask)
{-# INLINE unpackRef #-}

refDefIdx :: DefRef -> Int
refDefIdx = fst . unpackRef
{-# INLINE refDefIdx #-}

refRow :: DefRef -> Int
refRow = snd . unpackRef
{-# INLINE refRow #-}

--
-- The table (skeleton: row lifecycle only, no data columns yet)
--

data DefTable = DefTable
  { dt_index :: !(IORef (HashMap Hash128 Int))
  -- ^ this def's own param-hash -> row index (its share of what used to be
  -- one global intern table)
  , dt_free :: !(IORef [Int])
  , dt_len :: !(IORef Int)
  -- ^ logical row count (rows below this are either live, indexed by
  -- 'dt_index', or free, tracked by 'dt_free')
  }

new :: IO DefTable
new = DefTable <$> newIORef HashMap.empty <*> newIORef [] <*> newIORef 0

-- | The table's current logical row count (rows @0@ until this are valid
-- row numbers, though not all are necessarily allocated -- some may be on
-- the free list).
rowCount :: DefTable -> IO Int
rowCount dt = readIORef (dt_len dt)

allocRow :: DefTable -> IO Int
allocRow dt = do
  free <- readIORef (dt_free dt)
  case free of
    (r : rs) -> do
      writeIORef (dt_free dt) rs
      pure r
    [] -> do
      len <- readIORef (dt_len dt)
      writeIORef (dt_len dt) (len + 1)
      pure len

-- | Get-or-create the row for a given param hash. Returns 'True' as the
-- second component on a fresh row (newly allocated, possibly a recycled
-- row number from the free list), 'False' on a hit.
lookupOrInsertRow :: DefTable -> Hash128 -> IO (Int, Bool)
lookupOrInsertRow dt h = do
  idx <- readIORef (dt_index dt)
  case HashMap.lookup h idx of
    Just row -> pure (row, False)
    Nothing -> do
      row <- allocRow dt
      modifyIORef' (dt_index dt) (HashMap.insert h row)
      pure (row, True)

-- | Remove a row's hash-index entry and push its row number onto the free
-- list, making it available for reuse by a later 'lookupOrInsertRow'.
freeRow :: DefTable -> Hash128 -> Int -> IO ()
freeRow dt h row = do
  modifyIORef' (dt_index dt) (HashMap.delete h)
  modifyIORef' (dt_free dt) (row :)

--
-- Tests
--

h :: Int -> Hash128
h = largeHash128

test_packUnpackRoundTrips :: IO ()
test_packUnpackRoundTrips = do
  assertEqual (3, 7) (unpackRef (packRef 3 7))
  assertEqual (0, 0) (unpackRef (packRef 0 0))
  assertEqual (5, 0) (unpackRef (packRef 5 0))
  assertEqual 3 (refDefIdx (packRef 3 7))
  assertEqual 7 (refRow (packRef 3 7))

test_newTableIsEmpty :: IO ()
test_newTableIsEmpty = do
  dt <- new
  n <- rowCount dt
  assertEqual 0 n

test_lookupOrInsertRowIsIdempotent :: IO ()
test_lookupOrInsertRowIsIdempotent = do
  dt <- new
  (r1, fresh1) <- lookupOrInsertRow dt (h 1)
  (r2, fresh2) <- lookupOrInsertRow dt (h 1)
  assertEqual r1 r2
  assertBool fresh1
  assertBool (not fresh2)

test_lookupOrInsertRowDistinctHashesGetDistinctRows :: IO ()
test_lookupOrInsertRowDistinctHashesGetDistinctRows = do
  dt <- new
  (r1, _) <- lookupOrInsertRow dt (h 1)
  (r2, _) <- lookupOrInsertRow dt (h 2)
  assertBool (r1 /= r2)
  n <- rowCount dt
  assertEqual 2 n

test_freeRowThenReinsertNewHashReusesRowNumber :: IO ()
test_freeRowThenReinsertNewHashReusesRowNumber = do
  dt <- new
  (r1, _) <- lookupOrInsertRow dt (h 1)
  freeRow dt (h 1) r1
  (r2, fresh) <- lookupOrInsertRow dt (h 2)
  assertBool fresh
  assertEqual r1 r2
  -- freeing didn't grow the table -- the freed slot was reused, not a new one
  n <- rowCount dt
  assertEqual 1 n

test_freeRowRemovesOldHashFromIndex :: IO ()
test_freeRowRemovesOldHashFromIndex = do
  dt <- new
  (r1, _) <- lookupOrInsertRow dt (h 1)
  freeRow dt (h 1) r1
  -- looking the old hash up again must allocate a *fresh* row (it's gone
  -- from the index), even though the row number happens to be recyclable
  (r2, fresh) <- lookupOrInsertRow dt (h 1)
  assertBool fresh
  assertEqual r1 r2

test_manyRowsAreAllDistinctAndCounted :: IO ()
test_manyRowsAreAllDistinctAndCounted = do
  dt <- new
  rows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h i)) [1 .. 200]
  n <- rowCount dt
  assertEqual 200 n
  assertEqual 200 (length (HashMap.toList (toSet rows)))
 where
  toSet = HashMap.fromList . map (\r -> (r, ()))
