{-# LANGUAGE ScopedTypeVariables #-}
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

 = Columns

 Each column is a separate growable vector, unboxed where the element type
 allows it ('Data.Vector.Unboxed.Mutable' for @param_hash@/@result_hash@
 (as two 'Word64's each, since 'Hash128' itself has no 'Unbox' instance and
 splitting it avoids needing to write one) and @flags@), boxed where it
 can't be ('Data.Vector.Mutable' for the typed @param@/@value@ columns and
 for the edge columns). A boxed column's freshly-grown capacity is left as
 the @vector@ package's own "uninitialised element" error thunk (see
 'growBoxed') -- exactly the loud-failure-on-premature-read canary this
 module established for row lifecycle in an earlier increment (dead ids
 there, dead cells here), now extended to row storage. Growth for the
 @flags@ column is the one exception: it is explicitly zeroed (not left as
 unboxed garbage), because a stray nonzero byte there would silently read
 as an occupied row.

 = Row lifecycle and reuse

 A freed row is pushed onto a free list and its 'flags' cleared (not-alive,
 no result, not pending); every other column is left untouched -- stale
 param/value/edges from the previous occupant. This is safe under the
 invariant that every read of a possibly-stale column is gated behind a
 flags check (@alive@ for identity-ish columns, @hasResult@ for the
 value/result-hash columns) that is unconditionally cleared before the row
 can be reused, so the garbage can never be observed. Reuse does not
 recycle the row's *hash* index entry (that is removed by the caller before
 freeing) but does recycle the row *number* -- new occupants of a freed row
 get a fresh index entry pointing at the recycled number.
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
  readParam,
  readValue,
  writeValue,
  readCompDeps,
  writeCompDeps,
  compDepTargets,
  hashToPair,
  pairToHash,
  noResultSentinel,
  readRdeps,
  writeRdeps,
  readSrcDeps,
  writeSrcDeps,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSrc (AnyCompSrcDep)
import Control.Computations.Utils.Hash (Hash128 (..), largeHash128)

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Bits
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import qualified Data.LargeHashable as LH
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
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

-- | Sentinel pair for "observed no result" (a failed dependency), used in
-- the @compDeps@ column's observed-version slot. Colliding with this via a
-- real MD5-derived 'Hash128' is not a realistic concern.
noResultSentinel :: (Word64, Word64)
noResultSentinel = (maxBound, maxBound)

--
-- Generic growable-vector helpers (shared by boxed and unboxed columns via
-- Data.Vector.Generic.Mutable, which both implement)
--

-- | Grow a boxed column so it has room for at least @needed@ rows. Freshly
-- grown capacity is the @vector@ package's own uninitialised-element error
-- thunk -- reading it before writing is a loud crash, by design.
growBoxed :: IORef (VM.IOVector e) -> Int -> IO ()
growBoxed ref needed = do
  vec <- readIORef ref
  let cap = GM.length vec
  when (needed > cap) $ do
    vec' <- GM.unsafeGrow vec (max (needed - cap) (max 4 cap))
    writeIORef ref vec'
 where
  when b act = if b then act else pure ()

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
-- Used only for @flags@: a stray nonzero garbage byte there would silently
-- read as an occupied row, unlike every other unboxed column, which is
-- read only after a flags check has already gated it.
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

data DefTable p a = DefTable
  { dt_paramHash :: !(IORef (VUM.IOVector (Word64, Word64)))
  , dt_resultHash :: !(IORef (VUM.IOVector (Word64, Word64)))
  , dt_flags :: !(IORef (VUM.IOVector Word8))
  , dt_param :: !(IORef (VM.IOVector p))
  , dt_value :: !(IORef (VM.IOVector a))
  -- ^ valid only when 'flagsResultState' is 'ResultValue'
  , dt_compDeps :: !(IORef (VM.IOVector (VU.Vector (Int, Word64, Word64))))
  -- ^ flat forward comp-dep edges: (packed 'DefRef' this row depends on,
  -- the target's result hash *as observed* when this row last ran --
  -- needed to replicate the old VerList-based "impure cap" detection,
  -- which must distinguish "I depend on the same target at the same
  -- version as before, yet produced a different result" (genuinely
  -- impure) from "I depend on the same target but at a newly-changed
  -- version" (an ordinary, expected recompute) -- a target-set comparison
  -- alone can't tell those apart. A target with no result at observation
  -- time (a failed dependency) is encoded as the sentinel
  -- @(maxBound, maxBound)@; colliding with a real MD5-derived hash is not
  -- a realistic concern.
  , dt_rdeps :: !(IORef (VM.IOVector (VU.Vector Int)))
  -- ^ flat reverse comp-dep edges (packed 'DefRef's depending on this row)
  , dt_srcDeps :: !(IORef (VM.IOVector (HashSet AnyCompSrcDep)))
  , dt_index :: !(IORef (HashMap Hash128 Int))
  -- ^ this def's own param-hash -> row index (its share of what used to be
  -- one global intern table)
  , dt_free :: !(IORef [Int])
  , dt_len :: !(IORef Int)
  -- ^ logical row count (<= every column's current capacity)
  }

new :: IO (DefTable p a)
new = do
  ph <- newIORef =<< VUM.new 0
  rh <- newIORef =<< VUM.new 0
  fl <- newIORef =<< VUM.new 0
  pa <- newIORef =<< VM.new 0
  va <- newIORef =<< VM.new 0
  cd <- newIORef =<< VM.new 0
  rd <- newIORef =<< VM.new 0
  sd <- newIORef =<< VM.new 0
  ix <- newIORef HashMap.empty
  fr <- newIORef []
  ln <- newIORef 0
  pure
    DefTable
      { dt_paramHash = ph
      , dt_resultHash = rh
      , dt_flags = fl
      , dt_param = pa
      , dt_value = va
      , dt_compDeps = cd
      , dt_rdeps = rd
      , dt_srcDeps = sd
      , dt_index = ix
      , dt_free = fr
      , dt_len = ln
      }

growAllTo :: DefTable p a -> Int -> IO ()
growAllTo dt needed = do
  growUnboxed (dt_paramHash dt) needed
  growUnboxed (dt_resultHash dt) needed
  growUnboxedZeroed (dt_flags dt) needed
  growBoxed (dt_param dt) needed
  growBoxed (dt_value dt) needed
  growBoxed (dt_compDeps dt) needed
  growBoxed (dt_rdeps dt) needed
  growBoxed (dt_srcDeps dt) needed

-- | The table's current logical row count (rows 0 until this are valid
-- indices, though not all are necessarily alive -- see 'isAlive').
rowCount :: DefTable p a -> IO Int
rowCount dt = readIORef (dt_len dt)

allocRow :: DefTable p a -> IO Int
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
-- the param hash, the (caller-supplied) typed param, initializes flags to
-- alive/not-pending/no-result, resets the edge/src-dep columns to empty
-- (cheap, shared 'VU.empty'/'HashSet.empty' -- not per-row garbage, an
-- explicit reset, since a freshly *allocated* row -- as opposed to a
-- *reused* one -- has never had edges and there is nothing stale to gate),
-- and returns 'True' as the second component. Returns 'False' on a hit.
lookupOrInsertRow :: forall p a. DefTable p a -> Hash128 -> p -> IO (Int, Bool)
lookupOrInsertRow dt h p = do
  idx <- readIORef (dt_index dt)
  case HashMap.lookup h idx of
    Just row -> pure (row, False)
    Nothing -> do
      row <- allocRow dt
      modifyIORef' (dt_index dt) (HashMap.insert h row)
      writeHash (dt_paramHash dt) row h
      writeParam dt row p
      writeFlags dt row (mkFlags True False NoResult)
      writeCompDeps dt row VU.empty
      writeRdeps dt row VU.empty
      writeSrcDeps dt row HashSet.empty
      pure (row, True)

-- | Remove a row's hash-index entry and push it onto the free list,
-- clearing flags (not-alive, not-pending, no-result). Every other column
-- (param/value/edges/result hash) is left untouched -- see the module
-- haddock's "Row lifecycle and reuse" section for why that's safe.
freeRow :: DefTable p a -> Hash128 -> Int -> IO ()
freeRow dt h row = do
  modifyIORef' (dt_index dt) (HashMap.delete h)
  modifyIORef' (dt_free dt) (row :)
  writeFlags dt row 0

isAlive :: DefTable p a -> Int -> IO Bool
isAlive dt row = flagsAlive <$> readFlags dt row

--
-- Column access. All of these assume `row` is < the table's current
-- logical length; callers only ever get a `row` from `lookupOrInsertRow`
-- or from a `DefRef` that was itself minted that way, so this holds by
-- construction.
--

readFlags :: DefTable p a -> Int -> IO Flags
readFlags dt row = readIORef (dt_flags dt) >>= \v -> VUM.read v row

writeFlags :: DefTable p a -> Int -> Flags -> IO ()
writeFlags dt row f = readIORef (dt_flags dt) >>= \v -> VUM.write v row f

setPending :: DefTable p a -> Int -> Bool -> IO ()
setPending dt row p = do
  f <- readFlags dt row
  writeFlags dt row (mkFlags (flagsAlive f) p (flagsResultState f))

readHash :: IORef (VUM.IOVector (Word64, Word64)) -> Int -> IO Hash128
readHash ref row = pairToHash <$> (readIORef ref >>= \v -> VUM.read v row)

writeHash :: IORef (VUM.IOVector (Word64, Word64)) -> Int -> Hash128 -> IO ()
writeHash ref row hv = readIORef ref >>= \v -> VUM.write v row (hashToPair hv)

readParamHash :: DefTable p a -> Int -> IO Hash128
readParamHash dt = readHash (dt_paramHash dt)

readResultHash :: DefTable p a -> Int -> IO Hash128
readResultHash dt = readHash (dt_resultHash dt)

writeResultHash :: DefTable p a -> Int -> Hash128 -> IO ()
writeResultHash dt = writeHash (dt_resultHash dt)

-- | Zero out the result-hash slot. Not required for correctness (every
-- read is gated behind the result-state flag bits), but avoids a
-- changed-bit false-negative from comparing against an ancient hash if a
-- future column dump/debug tool ever reads it unconditionally.
clearResultHash :: DefTable p a -> Int -> IO ()
clearResultHash dt row = writeHash (dt_resultHash dt) row (pairToHash (0, 0))

writeParam :: DefTable p a -> Int -> p -> IO ()
writeParam dt row p = readIORef (dt_param dt) >>= \v -> VM.write v row p

readParam :: DefTable p a -> Int -> IO p
readParam dt row = readIORef (dt_param dt) >>= \v -> VM.read v row

readValue :: DefTable p a -> Int -> IO a
readValue dt row = readIORef (dt_value dt) >>= \v -> VM.read v row

writeValue :: DefTable p a -> Int -> a -> IO ()
writeValue dt row a = readIORef (dt_value dt) >>= \v -> VM.write v row a

readCompDeps :: DefTable p a -> Int -> IO (VU.Vector (Int, Word64, Word64))
readCompDeps dt row = readIORef (dt_compDeps dt) >>= \v -> VM.read v row

writeCompDeps :: DefTable p a -> Int -> VU.Vector (Int, Word64, Word64) -> IO ()
writeCompDeps dt row xs = readIORef (dt_compDeps dt) >>= \v -> VM.write v row xs

-- | Just the target refs of a comp-dep edge set, discarding the observed
-- version -- what the rdeps graph (add/remove edges, GC liveness) cares
-- about.
compDepTargets :: VU.Vector (Int, Word64, Word64) -> VU.Vector Int
compDepTargets = VU.map (\(r, _, _) -> r)

readRdeps :: DefTable p a -> Int -> IO (VU.Vector Int)
readRdeps dt row = readIORef (dt_rdeps dt) >>= \v -> VM.read v row

writeRdeps :: DefTable p a -> Int -> VU.Vector Int -> IO ()
writeRdeps dt row xs = readIORef (dt_rdeps dt) >>= \v -> VM.write v row xs

readSrcDeps :: DefTable p a -> Int -> IO (HashSet AnyCompSrcDep)
readSrcDeps dt row = readIORef (dt_srcDeps dt) >>= \v -> VM.read v row

writeSrcDeps :: DefTable p a -> Int -> HashSet AnyCompSrcDep -> IO ()
writeSrcDeps dt row s = readIORef (dt_srcDeps dt) >>= \v -> VM.write v row s

--
-- Tests
--

h :: Int -> Hash128
h = largeHash128

newTest :: IO (DefTable Int Int)
newTest = new

test_packUnpackRoundTrips :: IO ()
test_packUnpackRoundTrips = do
  assertEqual (3, 7) (unpackRef (packRef 3 7))
  assertEqual (0, 0) (unpackRef (packRef 0 0))
  assertEqual (5, 0) (unpackRef (packRef 5 0))
  assertEqual 3 (refDefIdx (packRef 3 7))
  assertEqual 7 (refRow (packRef 3 7))

test_newTableIsEmpty :: IO ()
test_newTableIsEmpty = do
  dt <- newTest
  n <- rowCount dt
  assertEqual 0 n

test_lookupOrInsertRowIsIdempotent :: IO ()
test_lookupOrInsertRowIsIdempotent = do
  dt <- newTest
  (r1, fresh1) <- lookupOrInsertRow dt (h 1) 100
  (r2, fresh2) <- lookupOrInsertRow dt (h 1) 100
  assertEqual r1 r2
  assertBool fresh1
  assertBool (not fresh2)

test_lookupOrInsertRowDistinctHashesGetDistinctRows :: IO ()
test_lookupOrInsertRowDistinctHashesGetDistinctRows = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  assertBool (r1 /= r2)
  n <- rowCount dt
  assertEqual 2 n

test_freeRowThenReinsertNewHashReusesRowNumber :: IO ()
test_freeRowThenReinsertNewHashReusesRowNumber = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  freeRow dt (h 1) r1
  (r2, fresh) <- lookupOrInsertRow dt (h 2) 2
  assertBool fresh
  assertEqual r1 r2
  -- freeing didn't grow the table -- the freed slot was reused, not a new one
  n <- rowCount dt
  assertEqual 1 n

test_freeRowRemovesOldHashFromIndex :: IO ()
test_freeRowRemovesOldHashFromIndex = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  freeRow dt (h 1) r1
  -- looking the old hash up again must allocate a *fresh* row (it's gone
  -- from the index), even though the row number happens to be recyclable
  (r2, fresh) <- lookupOrInsertRow dt (h 1) 1
  assertBool fresh
  assertEqual r1 r2

test_manyRowsAreAllDistinctAndCounted :: IO ()
test_manyRowsAreAllDistinctAndCounted = do
  dt <- newTest
  rows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h i) i) [1 .. 200]
  n <- rowCount dt
  assertEqual 200 n
  assertEqual 200 (length (HashMap.toList (toSet rows)))
 where
  toSet = HashMap.fromList . map (\r -> (r, ()))

test_freshRowIsAliveWithNoResult :: IO ()
test_freshRowIsAliveWithNoResult = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  alive <- isAlive dt r
  assertBool alive
  f <- readFlags dt r
  assertEqual NoResult (flagsResultState f)
  assertBool (not (flagsPending f))

test_freeRowIsNoLongerAlive :: IO ()
test_freeRowIsNoLongerAlive = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  freeRow dt (h 1) r
  alive <- isAlive dt r
  assertBool (not alive)

test_paramHashRoundTripsThroughLookupOrInsertRow :: IO ()
test_paramHashRoundTripsThroughLookupOrInsertRow = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 42) 42
  got <- readParamHash dt r
  assertEqual (h 42) got

test_resultHashRoundTrips :: IO ()
test_resultHashRoundTrips = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeResultHash dt r (h 99)
  got <- readResultHash dt r
  assertEqual (h 99) got

test_setPendingTogglesFlagWithoutDisturbingOthers :: IO ()
test_setPendingTogglesFlagWithoutDisturbingOthers = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
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
  dt <- newTest
  -- force several regrowths (initial/step capacity starts at 4)
  rows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h i) i) [1 .. 500]
  mapM_
    ( \(i, r) -> do
        ph <- readParamHash dt r
        assertEqual (h i) ph
        v <- readParam dt r
        assertEqual i v
        alive <- isAlive dt r
        assertBool alive
    )
    (zip [1 .. 500] rows)

test_valueRoundTrips :: IO ()
test_valueRoundTrips = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeValue dt r 12345
  v <- readValue dt r
  assertEqual (12345 :: Int) v

test_freshRowHasEmptyEdges :: IO ()
test_freshRowHasEmptyEdges = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  cd <- readCompDeps dt r
  rd <- readRdeps dt r
  sd <- readSrcDeps dt r
  assertEqual VU.empty cd
  assertEqual VU.empty rd
  assertEqual HashSet.empty sd

test_compDepsAndRdepsRoundTrip :: IO ()
test_compDepsAndRdepsRoundTrip = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  let cd = VU.fromList [(10, 1, 2), (20, 3, 4)]
  writeCompDeps dt r cd
  got <- readCompDeps dt r
  assertEqual cd got
  assertEqual (VU.fromList [10, 20]) (compDepTargets got)
  let rd = VU.fromList [30, 40, 50]
  writeRdeps dt r rd
  gotRd <- readRdeps dt r
  assertEqual rd gotRd

-- | The sentinel used for "no result observed" must not collide with any
-- real hash pair produced by 'hashToPair' -- spot-check a few concrete
-- hashes against it.
test_noResultSentinelDoesNotCollideWithRealHashes :: IO ()
test_noResultSentinelDoesNotCollideWithRealHashes =
  mapM_
    (\i -> assertBool (hashToPair (h i) /= noResultSentinel))
    [1 .. 50 :: Int]

--
-- Row reuse and garbage tolerance (Rust Stage 5's rules, ported): freeing a
-- row must not physically clear its result-hash/value/edge columns -- only
-- flags, param hash, and the caller-visible param/edge-reset fields that
-- lookupOrInsertRow re-establishes on the *next* occupant. Everything else
-- is left as garbage until overwritten, safe only because every read is
-- gated behind a flags check that reuse unconditionally clears first.
--

test_freeRowLeavesResultHashAndValueAsGarbageButFlagsGateThem :: IO ()
test_freeRowLeavesResultHashAndValueAsGarbageButFlagsGateThem = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 111
  writeResultHash dt r1 (h 999)
  writeValue dt r1 (777 :: Int)
  freeRow dt (h 1) r1
  -- the row is dead -- callers must not trust it without checking flags
  -- first (this module doesn't force-clear result_hash/value: reusing the
  -- row is what re-establishes a safe state, matching Rust Stage 5)
  alive <- isAlive dt r1
  assertBool (not alive)
  -- reuse: a fresh occupant of the same row number gets fresh flags/param,
  -- but the physical result_hash/value bytes are untouched garbage until
  -- this new occupant's own finish overwrites them -- readable-but-stale,
  -- never observed because nothing reads them without a flags check first.
  (r2, fresh) <- lookupOrInsertRow dt (h 2) 222
  assertBool fresh
  assertEqual r1 r2
  f <- readFlags dt r2
  assertEqual NoResult (flagsResultState f)
  assertBool (flagsAlive f)
  staleHash <- readResultHash dt r2
  assertEqual (h 999) staleHash -- old bytes, still physically there
  staleVal <- readValue dt r2
  assertEqual (777 :: Int) staleVal -- old bytes, still physically there
  -- but the *new* occupant's own param is correct, not garbage
  p <- readParam dt r2
  assertEqual (222 :: Int) p

test_freeRowResetsEdgesOnReuse :: IO ()
test_freeRowResetsEdgesOnReuse = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  writeCompDeps dt r1 (VU.fromList [(5, 1, 2)])
  writeRdeps dt r1 (VU.fromList [7, 8])
  freeRow dt (h 1) r1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  assertEqual r1 r2
  cd <- readCompDeps dt r2
  rd <- readRdeps dt r2
  assertEqual VU.empty cd
  assertEqual VU.empty rd
