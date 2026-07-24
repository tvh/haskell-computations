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

 = Columns (so far)

 @param_hash@/@result_hash@ (each two 'Word64's, since 'Hash128' itself has
 no 'Unbox' instance and splitting it avoids needing to write one) and
 @flags@ are genuinely unboxed columns ('Data.Vector.Unboxed.Mutable').
 Typed param/value columns and edge columns are later increments.

 A freshly grown region of an unboxed column is normally left as garbage
 bytes (cheap, and safe because every column here is read only behind a
 flags check) -- except @flags@ itself, whose growth is explicitly zeroed:
 a stray nonzero garbage byte there would silently read as an occupied
 row, the one column that can't rely on "gated behind a flags check"
 because it *is* the flags check.

 = Row lifecycle and reuse

 A freed row is pushed onto a free list and its 'Flags' cleared
 (not-alive, no result, not pending); every other column is left
 untouched. This is safe under the invariant every read of a possibly-
 stale column is gated behind a flags check ('flagsAlive' or the result-
 state bits), unconditionally cleared before the row can be reused.
-}
module Control.Computations.CompEngine.Utils.DefTable (
  -- * Row identity
  DefRef,
  packRef,
  unpackRef,
  refDefIdx,
  refRow,

  -- * Flags
  Flags,
  ResultState (..),
  flagsAlive,
  flagsPending,
  flagsResultState,
  mkFlags,

  -- * The table
  DefTable,
  new,
  rowCount,

  -- * Row lifecycle
  lookupOrInsertRow,
  freeRow,
  isAlive,

  -- * Column access
  readFlags,
  writeFlags,
  setPending,
  readParamHash,
  readResultHash,
  writeResultHash,
  clearResultHash,
  hashToPair,
  pairToHash,
  noResultSentinel,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Hash (Hash128 (..), largeHash128)

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Bits
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.LargeHashable as LH
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word64, Word8)
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
-- Flags
--

-- | Packed per-row state: bit 0 alive, bit 1 pending (mid-evaluation),
-- bits 2-3 result state (see 'ResultState').
type Flags = Word8

data ResultState = NoResult | ResultFailure | ResultValue | ResultMetaOnly
  deriving (Eq, Show, Enum, Bounded)

bitAlive, bitPending :: Int
bitAlive = 0
bitPending = 1

flagsAlive :: Flags -> Bool
flagsAlive f = testBit f bitAlive
{-# INLINE flagsAlive #-}

flagsPending :: Flags -> Bool
flagsPending f = testBit f bitPending
{-# INLINE flagsPending #-}

flagsResultState :: Flags -> ResultState
flagsResultState f = toEnum (fromIntegral ((f `shiftR` 2) .&. 0x3))
{-# INLINE flagsResultState #-}

mkFlags :: Bool -> Bool -> ResultState -> Flags
mkFlags alive pending rs =
  (if alive then bit bitAlive else 0)
    .|. (if pending then bit bitPending else 0)
    .|. (fromIntegral (fromEnum rs) `shiftL` 2)
{-# INLINE mkFlags #-}

--
-- Hash128 <-> unboxed pair
--

hashToPair :: Hash128 -> (Word64, Word64)
hashToPair (Hash128 w) = (LH.w128_first w, LH.w128_second w)
{-# INLINE hashToPair #-}

pairToHash :: (Word64, Word64) -> Hash128
pairToHash (a, b) = Hash128 (LH.Word128 a b)
{-# INLINE pairToHash #-}

-- | Sentinel pair for "observed no result" (a failed dependency), used by
-- later increments' comp-dep edge column. Colliding with this via a real
-- MD5-derived 'Hash128' is not a realistic concern.
noResultSentinel :: (Word64, Word64)
noResultSentinel = (maxBound, maxBound)

--
-- Generic growable-vector helpers, unboxed columns only so far
--

-- | Grow an unboxed column so it has room for at least @needed@ rows.
-- Freshly grown capacity is unspecified garbage bytes -- safe only because
-- every unboxed column here is read strictly behind a flags check.
growUnboxed :: VUM.Unbox e => IORef (VUM.IOVector e) -> Int -> IO ()
growUnboxed ref needed = do
  vec <- readIORef ref
  let cap = GM.length vec
  when (needed > cap) $ do
    vec' <- GM.unsafeGrow vec (max (needed - cap) (max 4 cap))
    writeIORef ref vec'
 where
  when b act = if b then act else pure ()

-- | Like 'growUnboxed', but explicitly zeroes the freshly grown region.
-- Used only for @flags@: a stray nonzero garbage byte there would
-- silently read as an occupied row, unlike every other unboxed column,
-- which is read only after a flags check has already gated it.
growUnboxedZeroed :: IORef (VUM.IOVector Word8) -> Int -> IO ()
growUnboxedZeroed ref needed = do
  vec <- readIORef ref
  let cap = GM.length vec
  when (needed > cap) $ do
    let extra = max (needed - cap) (max 4 cap)
    vec' <- GM.unsafeGrow vec extra
    GM.set (GM.unsafeSlice cap extra vec') 0
    writeIORef ref vec'
 where
  when b act = if b then act else pure ()

--
-- The table
--

data DefTable = DefTable
  { dt_paramHash :: !(IORef (VUM.IOVector (Word64, Word64)))
  , dt_resultHash :: !(IORef (VUM.IOVector (Word64, Word64)))
  , dt_flags :: !(IORef (VUM.IOVector Word8))
  , dt_index :: !(IORef (HashMap Hash128 Int))
  -- ^ this def's own param-hash -> row index (its share of what used to be
  -- one global intern table)
  , dt_free :: !(IORef [Int])
  , dt_len :: !(IORef Int)
  -- ^ logical row count (<= every column's current capacity)
  }

new :: IO DefTable
new = do
  ph <- newIORef =<< VUM.new 0
  rh <- newIORef =<< VUM.new 0
  fl <- newIORef =<< VUM.new 0
  ix <- newIORef HashMap.empty
  fr <- newIORef []
  ln <- newIORef 0
  pure DefTable{dt_paramHash = ph, dt_resultHash = rh, dt_flags = fl, dt_index = ix, dt_free = fr, dt_len = ln}

growAllTo :: DefTable -> Int -> IO ()
growAllTo dt needed = do
  growUnboxed (dt_paramHash dt) needed
  growUnboxed (dt_resultHash dt) needed
  growUnboxedZeroed (dt_flags dt) needed

-- | The table's current logical row count (rows @0@ until this are valid
-- row numbers, though not all are necessarily alive -- see 'isAlive').
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
      growAllTo dt (len + 1)
      writeIORef (dt_len dt) (len + 1)
      pure len

-- | Get-or-create the row for a given param hash. On a fresh row, writes
-- the param hash and initializes flags to alive/not-pending/no-result.
-- Returns 'True' as the second component on a fresh row, 'False' on a hit.
lookupOrInsertRow :: DefTable -> Hash128 -> IO (Int, Bool)
lookupOrInsertRow dt h = do
  idx <- readIORef (dt_index dt)
  case HashMap.lookup h idx of
    Just row -> pure (row, False)
    Nothing -> do
      row <- allocRow dt
      modifyIORef' (dt_index dt) (HashMap.insert h row)
      writeHash (dt_paramHash dt) row h
      writeFlags dt row (mkFlags True False NoResult)
      pure (row, True)

-- | Remove a row's hash-index entry and push it onto the free list,
-- clearing flags (not-alive, not-pending, no-result). Every other column
-- is left untouched -- see the module haddock's "Row lifecycle and reuse"
-- section for why that's safe.
freeRow :: DefTable -> Hash128 -> Int -> IO ()
freeRow dt h row = do
  modifyIORef' (dt_index dt) (HashMap.delete h)
  modifyIORef' (dt_free dt) (row :)
  writeFlags dt row 0

isAlive :: DefTable -> Int -> IO Bool
isAlive dt row = flagsAlive <$> readFlags dt row

--
-- Column access. All of these assume `row` is < the table's current
-- logical length; callers only ever get a `row` from `lookupOrInsertRow`
-- or from a `DefRef` that was itself minted that way, so this holds by
-- construction.
--

readFlags :: DefTable -> Int -> IO Flags
readFlags dt row = readIORef (dt_flags dt) >>= \v -> VUM.read v row

writeFlags :: DefTable -> Int -> Flags -> IO ()
writeFlags dt row f = readIORef (dt_flags dt) >>= \v -> VUM.write v row f

setPending :: DefTable -> Int -> Bool -> IO ()
setPending dt row p = do
  f <- readFlags dt row
  writeFlags dt row (mkFlags (flagsAlive f) p (flagsResultState f))

readHash :: IORef (VUM.IOVector (Word64, Word64)) -> Int -> IO Hash128
readHash ref row = pairToHash <$> (readIORef ref >>= \v -> VUM.read v row)

writeHash :: IORef (VUM.IOVector (Word64, Word64)) -> Int -> Hash128 -> IO ()
writeHash ref row hv = readIORef ref >>= \v -> VUM.write v row (hashToPair hv)

readParamHash :: DefTable -> Int -> IO Hash128
readParamHash dt = readHash (dt_paramHash dt)

readResultHash :: DefTable -> Int -> IO Hash128
readResultHash dt = readHash (dt_resultHash dt)

writeResultHash :: DefTable -> Int -> Hash128 -> IO ()
writeResultHash dt = writeHash (dt_resultHash dt)

-- | Zero out the result-hash slot. Not required for correctness (every
-- read is gated behind the result-state flag bits), but avoids a
-- changed-bit false-negative from comparing against an ancient hash if a
-- future column dump/debug tool ever reads it unconditionally.
clearResultHash :: DefTable -> Int -> IO ()
clearResultHash dt row = writeHash (dt_resultHash dt) row (pairToHash (0, 0))

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

test_freshRowIsAliveWithNoResult :: IO ()
test_freshRowIsAliveWithNoResult = do
  dt <- new
  (r, _) <- lookupOrInsertRow dt (h 1)
  alive <- isAlive dt r
  assertBool alive
  f <- readFlags dt r
  assertEqual NoResult (flagsResultState f)
  assertBool (not (flagsPending f))

test_freeRowIsNoLongerAlive :: IO ()
test_freeRowIsNoLongerAlive = do
  dt <- new
  (r, _) <- lookupOrInsertRow dt (h 1)
  freeRow dt (h 1) r
  alive <- isAlive dt r
  assertBool (not alive)

test_paramHashRoundTripsThroughLookupOrInsertRow :: IO ()
test_paramHashRoundTripsThroughLookupOrInsertRow = do
  dt <- new
  (r, _) <- lookupOrInsertRow dt (h 42)
  got <- readParamHash dt r
  assertEqual (h 42) got

test_resultHashRoundTrips :: IO ()
test_resultHashRoundTrips = do
  dt <- new
  (r, _) <- lookupOrInsertRow dt (h 1)
  writeResultHash dt r (h 99)
  got <- readResultHash dt r
  assertEqual (h 99) got

test_setPendingTogglesFlagWithoutDisturbingOthers :: IO ()
test_setPendingTogglesFlagWithoutDisturbingOthers = do
  dt <- new
  (r, _) <- lookupOrInsertRow dt (h 1)
  writeFlags dt r (mkFlags True False ResultValue)
  setPending dt r True
  f <- readFlags dt r
  assertBool (flagsAlive f)
  assertBool (flagsPending f)
  assertEqual ResultValue (flagsResultState f)
  setPending dt r False
  f' <- readFlags dt r
  assertBool (not (flagsPending f'))

test_mkFlagsResultStateRoundTripsForEveryValue :: IO ()
test_mkFlagsResultStateRoundTripsForEveryValue =
  mapM_
    ( \rs -> do
        let f = mkFlags True True rs
        assertEqual rs (flagsResultState f)
        assertBool (flagsAlive f)
        assertBool (flagsPending f)
    )
    [minBound .. maxBound]

test_hashPairRoundTrips :: IO ()
test_hashPairRoundTrips = do
  assertEqual (h 7) (pairToHash (hashToPair (h 7)))

test_growthAcrossManyInsertsPreservesAllData :: IO ()
test_growthAcrossManyInsertsPreservesAllData = do
  dt <- new
  -- force several regrowths (initial/step capacity starts at 4)
  rows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h i)) [1 .. 500]
  mapM_
    ( \(i, r) -> do
        ph <- readParamHash dt r
        assertEqual (h i) ph
        alive <- isAlive dt r
        assertBool alive
    )
    (zip [1 .. 500] rows)
