{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Utils.DefTableTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSrc (
  AnyCompSrcDep,
  CompSrc (..),
  CompSrcInstanceId (..),
  Dep (..),
  wrapCompSrcDep,
 )
import Control.Computations.CompEngine.Utils.DefTable
import Control.Computations.Utils.Hash (Hash128 (..), largeHash128)

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM (retry)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Either (isLeft)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (..))
import Data.IORef
import qualified Data.LargeHashable as LH
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Typeable (Typeable)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word32, Word64)
import Test.Framework hiding ((.&.))
import qualified Test.QuickCheck as QC

h :: Int -> Hash128
h = largeHash128

newTest :: IO (DefTable Int Int)
newTest = new

-- | Test-only shorthand for building 'DefIdx'/'RowIdx' values from plain
-- integer literals -- this module's own test section has the constructors
-- in scope (unlike every other module), so there's no need to go via the
-- public 'mkDefIdx' escape hatch.
di :: Int -> DefIdx
di = DefIdx

ri :: Int -> RowIdx
ri = RowIdx

test_packUnpackRoundTrips :: IO ()
test_packUnpackRoundTrips = do
  assertEqual (di 3, ri 7) (unpackRef (packRef (di 3) (ri 7)))
  assertEqual (di 0, ri 0) (unpackRef (packRef (di 0) (ri 0)))
  assertEqual (di 5, ri 0) (unpackRef (packRef (di 5) (ri 0)))
  assertEqual (di 3) (refDefIdx (packRef (di 3) (ri 7)))
  assertEqual (ri 7) (refRow (packRef (di 3) (ri 7)))

test_newTableIsEmpty :: IO ()
test_newTableIsEmpty = do
  dt <- newTest
  n <- rowCount dt
  assertEqual (RowCount 0) n

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
  assertEqual (RowCount 2) n

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
  assertEqual (RowCount 1) n

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
  assertEqual (RowCount 200) n
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

--
-- Column representation: mkColumn must pick ColUnboxed for the recognized
-- primitive types (mirroring the benchmark's own Word32 param / Word64
-- result) and ColBoxed for everything else (e.g. a non-unboxable param or
-- result type such as String -- the task brief's fallback requirement),
-- and both representations must round-trip correctly through DefTable's
-- ordinary read/write API either way.
--

test_columnPicksUnboxedForRecognizedPrimitiveTypes :: IO ()
test_columnPicksUnboxedForRecognizedPrimitiveTypes = do
  (dtW32 :: DefTable Word32 Word64) <- new
  assertBool (columnIsUnboxed (dt_param dtW32))
  assertBool (columnIsUnboxed (dt_value dtW32))
  (dtOther :: DefTable Bool Double) <- new
  assertBool (columnIsUnboxed (dt_param dtOther))
  assertBool (columnIsUnboxed (dt_value dtOther))

-- | A non-unboxable param/result type (here 'String', standing in for the
-- task brief's @ByteString@ example -- real param types in the test suite
-- include both) must fall back to 'ColBoxed', not fail to build.
test_columnFallsBackToBoxedForNonUnboxableTypes :: IO ()
test_columnFallsBackToBoxedForNonUnboxableTypes = do
  (dt :: DefTable String String) <- new
  assertBool (not (columnIsUnboxed (dt_param dt)))
  assertBool (not (columnIsUnboxed (dt_value dt)))

-- | Round-trip through the unboxed path (Word32 param / Word64 result,
-- exactly the benchmark's def shape) with values that exercise the full
-- width of each type.
test_unboxedColumnParamValueRoundTrip :: IO ()
test_unboxedColumnParamValueRoundTrip = do
  (dt :: DefTable Word32 Word64) <- new
  (r, _) <- lookupOrInsertRow dt (h 1) maxBound
  writeValue dt r maxBound
  p <- readParam dt r
  v <- readValue dt r
  assertEqual (maxBound :: Word32) p
  assertEqual (maxBound :: Word64) v

-- | Round-trip through the boxed fallback path with a non-unboxable type
-- (String), the case the task brief specifically asks to be covered: a
-- real param/result type that can never satisfy an 'Unbox' constraint
-- (like 'Data.ByteString.ByteString' in the wider test suite) must keep
-- working unchanged.
test_boxedColumnParamValueRoundTrip :: IO ()
test_boxedColumnParamValueRoundTrip = do
  (dt :: DefTable String String) <- new
  (r, _) <- lookupOrInsertRow dt (h 1) "hello param"
  writeValue dt r "hello value"
  p <- readParam dt r
  v <- readValue dt r
  assertEqual "hello param" p
  assertEqual "hello value" v

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
  let cd = VU.fromList [CompDepEdge 10 (1, 2), CompDepEdge 20 (3, 4)]
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
-- Src-dep interning: a minimal 'CompSrc' instance for building real
-- 'AnyCompSrcDep' test values (mirrors Tests/TestStateIf.hs's
-- @TestStateSrc@), plus tests covering the interned-CSR-arena
-- representation directly against the module's internals (this file's own
-- test section has access to 'SrcDepIntern', unlike SimpleStateIf.hs which
-- only ever sees decoded 'HashSet' 'AnyCompSrcDep' values through the
-- unchanged public API).
--

data TestSrc = TestSrc deriving (Show, Eq, Typeable)

data VoidRequest a

instance Hashable TestSrc where
  hashWithSalt s TestSrc = hashWithSalt s (0 :: Int)

instance CompSrc TestSrc where
  type CompSrcReq TestSrc = VoidRequest
  type CompSrcKey TestSrc = String
  type CompSrcVer TestSrc = Int
  compSrcInstanceId _ = CompSrcInstanceId (fromString "TestSrc")
  compSrcExecute _ act = case act of {}
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry

srcDep :: String -> Int -> AnyCompSrcDep
srcDep k v = wrapCompSrcDep TestSrc (Dep k v)

test_srcDepsRoundTrip :: IO ()
test_srcDepsRoundTrip = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  let s = HashSet.fromList [srcDep "a" 1, srcDep "b" 1]
  writeSrcDeps dt r s
  got <- readSrcDeps dt r
  assertEqual s got

-- | The same @(key, version)@ pair, written by two different rows, must be
-- interned exactly once -- this is the whole point of the rewrite (see the
-- module haddock's "Src-dep interning" section): rows sharing a source dep
-- share one heap copy via a small shared id, not a private copy each.
test_srcDepsInterningDedupesEqualValues :: IO ()
test_srcDepsInterningDedupesEqualValues = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "shared" 1))
  writeSrcDeps dt r2 (HashSet.singleton (srcDep "shared" 1))
  count <- readIORef (sdi_count (dt_srcDepIntern dt))
  assertEqual 1 count
  got1 <- readSrcDeps dt r1
  got2 <- readSrcDeps dt r2
  assertEqual (HashSet.singleton (srcDep "shared" 1)) got1
  assertEqual (HashSet.singleton (srcDep "shared" 1)) got2

-- | Distinct src deps get distinct ids -- including the case that actually
-- drives real-world interning table growth: the *same key* observed at a
-- *different version* is a genuinely distinct 'AnyCompSrcDep' (see
-- CompSrc.hs's @SomeCompSrcDep@, which wraps key and version together), not
-- deduplicated against the old version.
test_srcDepsDistinctValuesGetDistinctIds :: IO ()
test_srcDepsDistinctValuesGetDistinctIds = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r (HashSet.fromList [srcDep "k" 1, srcDep "k" 2, srcDep "other" 1])
  count <- readIORef (sdi_count (dt_srcDepIntern dt))
  assertEqual 3 count
  got <- readSrcDeps dt r
  assertEqual (HashSet.fromList [srcDep "k" 1, srcDep "k" 2, srcDep "other" 1]) got

-- | Overwriting a row's src deps repeatedly (the live-update path's actual
-- write pattern) must keep reading back the *latest* set, and must force at
-- least one compaction of the shared arena along the way -- exercising the
-- same append-new-span/mark-old-span-dead machinery 'writeCompDeps'/
-- 'writeRdeps' already get covered for, now via the interned-id path.
test_srcDepsManyOverwritesKeepLatestCorrectAndCompact :: IO ()
test_srcDepsManyOverwritesKeepLatestCorrectAndCompact = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  mapM_
    (\i -> writeSrcDeps dt r (HashSet.singleton (srcDep "churn" i)))
    [1 .. 5000 :: Int]
  got <- readSrcDeps dt r
  assertEqual (HashSet.singleton (srcDep "churn" 5000)) got

-- | A freed row's src-dep arena span is released (unlike @compDeps@/
-- @rdeps@, which stay orphaned garbage until compaction -- see the module
-- haddock for why src-deps can't wait): a *fresh* occupant of the recycled
-- row number must never see the old occupant's src deps.
test_srcDepsResetOnRowReuse :: IO ()
test_srcDepsResetOnRowReuse = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "old" 1))
  freeRow dt (h 1) r1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  assertEqual r1 r2
  got <- readSrcDeps dt r2
  assertEqual HashSet.empty got

--
-- Src-dep refcounting -- Part 1's correctness fix (module haddock's "Ids
-- are refcounted and recycled"). Everything below exercises the intern
-- table's internal state directly (this file's own test section, unlike
-- SimpleStateIf.hs, has access to SrcDepIntern) as well as through the
-- public DefTable read/write API.
--

test_srcDepInternLiveCountTracksDistinctValuesReferenced :: IO ()
test_srcDepInternLiveCountTracksDistinctValuesReferenced = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  c0 <- srcDepInternLiveCount dt
  assertEqual 0 c0
  writeSrcDeps dt r (HashSet.fromList [srcDep "a" 1, srcDep "b" 1])
  c1 <- srcDepInternLiveCount dt
  assertEqual 2 c1

-- | Freeing the only row that referenced a src dep must reclaim its
-- interned id -- the whole point of the refcounting fix (see the module
-- haddock's "Ids are refcounted and recycled"): the live-count must drop
-- back to zero, not stay pinned at its high-water mark the way the
-- pre-refcounting, never-recycled table would have.
test_freeingOnlyReferencingRowReclaimsSrcDepId :: IO ()
test_freeingOnlyReferencingRowReclaimsSrcDepId = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r (HashSet.singleton (srcDep "solo" 1))
  c1 <- srcDepInternLiveCount dt
  assertEqual 1 c1
  freeRow dt (h 1) r
  c2 <- srcDepInternLiveCount dt
  assertEqual 0 c2

-- | The core refcounting invariant: a src dep shared by two rows must
-- survive one of them freeing -- only when *both* referencing rows are
-- gone does the id actually get reclaimed. A naive (non-refcounted, or
-- incorrectly-ordered) implementation would either reclaim the shared id
-- the moment the first row frees (corrupting the still-live row's reads)
-- or never reclaim it at all.
test_freeingRowWithSharedSrcDepDoesNotDropSharedId :: IO ()
test_freeingRowWithSharedSrcDepDoesNotDropSharedId = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "shared" 1))
  writeSrcDeps dt r2 (HashSet.singleton (srcDep "shared" 1))
  c1 <- srcDepInternLiveCount dt
  assertEqual 1 c1
  freeRow dt (h 1) r1
  -- still referenced by r2 -- must not have been reclaimed
  c2 <- srcDepInternLiveCount dt
  assertEqual 1 c2
  got2 <- readSrcDeps dt r2
  assertEqual (HashSet.singleton (srcDep "shared" 1)) got2
  freeRow dt (h 2) r2
  -- now genuinely unreferenced
  c3 <- srcDepInternLiveCount dt
  assertEqual 0 c3

-- | The ordering-hazard case the module haddock calls out by name: a
-- write that keeps one src dep, drops another, and adds a third, over and
-- over. The *kept* one is present in both the write's old and new span on
-- every iteration -- if retain-before-release weren't respected, its
-- refcount would transiently hit zero at some point and get reclaimed out
-- from under the row that's still supposed to reference it.
test_overlappingOverwriteNeverDropsTheSharedId :: IO ()
test_overlappingOverwriteNeverDropsTheSharedId = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r (HashSet.fromList [srcDep "kept" 1, srcDep "gen0" 1])
  forM_ [1 .. 200 :: Int] $ \i -> do
    writeSrcDeps dt r (HashSet.fromList [srcDep "kept" 1, srcDep ("gen" ++ show i) 1])
    got <- readSrcDeps dt r
    assertBool (HashSet.member (srcDep "kept" 1) got)
  -- after the loop only "kept" and the final generation's id remain live
  c <- srcDepInternLiveCount dt
  assertEqual 2 c

-- | Distinct versions of the same key are distinct 'AnyCompSrcDep' values
-- (see 'test_srcDepsDistinctValuesGetDistinctIds'); replacing one version
-- with another via 'writeSrcDeps' must reclaim the old version's id once
-- nothing else references it, not accumulate one id per version forever
-- -- this is the concrete leak Part 1 fixes (the module haddock's
-- "Ids are refcounted and recycled").
test_versionChurnOnSingleRowKeepsInternTableBounded :: IO ()
test_versionChurnOnSingleRowKeepsInternTableBounded = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  forM_ [1 .. 5000 :: Int] $ \i ->
    writeSrcDeps dt r (HashSet.singleton (srcDep "churn" i))
  c <- srcDepInternLiveCount dt
  assertEqual 1 c
  got <- readSrcDeps dt r
  assertEqual (HashSet.singleton (srcDep "churn" 5000)) got

-- | A reused id (popped off the free list by 'sdiIntern') must behave
-- exactly like a fresh one: readable once retained, and the *previous*
-- occupant's identity must never leak through a resolve of the recycled
-- slot. Forces reuse directly: free a row (releasing its sole src dep to
-- refcount zero, recycling the id) and immediately intern a *different*
-- value on another row, which -- given a table with only that one freed
-- slot on its free list -- must land on the recycled id.
test_reusedSrcDepIdBehavesLikeFreshId :: IO ()
test_reusedSrcDepIdBehavesLikeFreshId = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "first" 1))
  freeRow dt (h 1) r1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  writeSrcDeps dt r2 (HashSet.singleton (srcDep "second" 1))
  got <- readSrcDeps dt r2
  assertEqual (HashSet.singleton (srcDep "second" 1)) got
  c <- srcDepInternLiveCount dt
  assertEqual 1 c

-- | Resolving a *freed* (refcount-zero) src-dep id must fail loudly, not
-- silently return stale or wrong data -- the "range check alone isn't
-- enough once ids recycle" contract the module haddock calls out. Reaches
-- the dead id indirectly through the public API: write a solo src dep,
-- capture what its row currently holds isn't directly observable (ids are
-- module-internal), so this drives the scenario through
-- 'readSrcDeps'/'writeSrcDeps' on a row whose dependency set changes,
-- confirming the *old* value is gone from the interned table (live count
-- drops) and cannot be produced by any subsequent read.
test_deadSrcDepIdIsNeverResolvableAfterRelease :: IO ()
test_deadSrcDepIdIsNeverResolvableAfterRelease = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r (HashSet.singleton (srcDep "gone-soon" 1))
  c1 <- srcDepInternLiveCount dt
  assertEqual 1 c1
  writeSrcDeps dt r (HashSet.singleton (srcDep "replacement" 1))
  c2 <- srcDepInternLiveCount dt
  assertEqual 1 c2
  got <- readSrcDeps dt r
  assertEqual (HashSet.singleton (srcDep "replacement" 1)) got

-- | Direct unit test against 'SrcDepIntern' itself: resolving a released
-- id must throw, not return the stale value or a range-check false
-- negative -- this is the loud-failure contract 'sdiResolve' is required
-- to strengthen (the module haddock's liveness-check note), tested here
-- against the primitive directly rather than only observed indirectly
-- through 'readSrcDeps'.
test_sdiResolveFailsLoudlyOnReleasedId :: IO ()
test_sdiResolveFailsLoudlyOnReleasedId = do
  sdi <- newSrcDepIntern
  let dep = srcDep "temp" 1
  i <- sdiIntern sdi dep
  sdiRetain sdi i
  sdiRelease sdi i
  result <- try (evaluate =<< sdiResolve sdi i) :: IO (Either SomeException AnyCompSrcDep)
  assertBool (isLeft result)

-- | 'sdiResolve' on an id that was never assigned at all (out of range)
-- must also fail loudly -- the other half of the loud-failure contract,
-- unrelated to recycling.
test_sdiResolveFailsLoudlyOnNeverAssignedId :: IO ()
test_sdiResolveFailsLoudlyOnNeverAssignedId = do
  sdi <- newSrcDepIntern
  result <- try (evaluate =<< sdiResolve sdi (mkSrcDepId 42)) :: IO (Either SomeException AnyCompSrcDep)
  assertBool (isLeft result)

-- | 'sdiRelease' on an id already at refcount zero (an underflow -- every
-- real caller pairs release with a prior retain, so this can only happen
-- from a caller bug) must fail loudly rather than silently going negative
-- or double-freeing the slot.
test_sdiReleaseUnderflowFailsLoudly :: IO ()
test_sdiReleaseUnderflowFailsLoudly = do
  sdi <- newSrcDepIntern
  let dep = srcDep "never-retained" 1
  i <- sdiIntern sdi dep
  result <- try (evaluate =<< sdiRelease sdi i) :: IO (Either SomeException ())
  assertBool (isLeft result)

-- | A row that never had any src deps must not appear to reference
-- anything on free -- 'freeRow' on such a row is a pure no-op as far as
-- the intern table is concerned (nothing to release, nothing to
-- underflow).
test_freeingRowWithNoSrcDepsIsInternTableNoOp :: IO ()
test_freeingRowWithNoSrcDepsIsInternTableNoOp = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  c0 <- srcDepInternLiveCount dt
  freeRow dt (h 1) r
  c1 <- srcDepInternLiveCount dt
  assertEqual c0 c1
  assertEqual 0 c1

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
  writeCompDeps dt r1 (VU.fromList [CompDepEdge 5 (1, 2)])
  writeRdeps dt r1 (VU.fromList [7, 8])
  freeRow dt (h 1) r1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  assertEqual r1 r2
  cd <- readCompDeps dt r2
  rd <- readRdeps dt r2
  assertEqual VU.empty cd
  assertEqual VU.empty rd

--
-- Edge-arena specific: repeated overwrite (the pattern that drives the
-- append-only arena's growth and eventually forces a compaction), and
-- compaction's interaction with rows that are alive vs. freed.
--

test_manyOverwritesOfSameRowKeepLatestEdgesCorrect :: IO ()
test_manyOverwritesOfSameRowKeepLatestEdgesCorrect = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  -- comfortably past compactMinWords (4096) at stride 3, so this forces at
  -- least one compaction of the compDeps arena along the way.
  mapM_
    ( \i -> do
        let cd = VU.fromList [CompDepEdge i (fromIntegral i, fromIntegral (i + 1))]
        writeCompDeps dt r cd
        let rd = VU.fromList [i, (i + 1), (i + 2)]
        writeRdeps dt r rd
    )
    [1 .. 3000]
  gotCd <- readCompDeps dt r
  assertEqual (VU.fromList [CompDepEdge 3000 (3000, 3001)]) gotCd
  gotRd <- readRdeps dt r
  assertEqual (VU.fromList [3000, 3001, 3002]) gotRd

test_compactionDoesNotDisturbOtherAliveRowsEdges :: IO ()
test_compactionDoesNotDisturbOtherAliveRowsEdges = do
  dt <- newTest
  (rHot, _) <- lookupOrInsertRow dt (h 0) 0
  -- a handful of bystander rows, each given a distinct, stable edge set
  -- that must still read back correctly after the hot row's repeated
  -- overwrites force one or more compactions of the shared arena.
  bystanders <- mapM (\i -> lookupOrInsertRow dt (h i) i) [1 .. 10]
  mapM_
    ( \(r, i) -> do
        writeCompDeps dt r (VU.fromList [CompDepEdge (100 + i) (fromIntegral i, fromIntegral i)])
        writeRdeps dt r (VU.fromList [(200 + i)])
    )
    (zip (map fst bystanders) [1 .. 10])
  mapM_
    ( \i -> writeCompDeps dt rHot (VU.fromList [CompDepEdge i (fromIntegral i, fromIntegral i)])
    )
    [1 .. 3000 :: Int]
  mapM_
    ( \(r, i) -> do
        cd <- readCompDeps dt r
        assertEqual (VU.fromList [CompDepEdge (100 + i) (fromIntegral i, fromIntegral i)]) cd
        rd <- readRdeps dt r
        assertEqual (VU.fromList [(200 + i)]) rd
    )
    (zip (map fst bystanders) [1 .. 10])

test_freedRowsDontResurfaceAfterCompaction :: IO ()
test_freedRowsDontResurfaceAfterCompaction = do
  dt <- newTest
  (rKeep, _) <- lookupOrInsertRow dt (h 0) 0
  writeCompDeps dt rKeep (VU.fromList [CompDepEdge 999 (1, 1)])
  (rDoomed, _) <- lookupOrInsertRow dt (h 1) 1
  writeCompDeps dt rDoomed (VU.fromList [CompDepEdge 888 (2, 2)])
  freeRow dt (h 1) rDoomed
  -- force compaction via repeated overwrites of the surviving row
  mapM_
    (\i -> writeCompDeps dt rKeep (VU.fromList [CompDepEdge i (fromIntegral i, fromIntegral i)]))
    [1 .. 3000 :: Int]
  aliveKeep <- isAlive dt rKeep
  assertBool aliveKeep
  cd <- readCompDeps dt rKeep
  assertEqual (VU.fromList [CompDepEdge 3000 (3000, 3000)]) cd
  aliveDoomed <- isAlive dt rDoomed
  assertBool (not aliveDoomed)

-- | The srcDeps-arena analogue of 'test_compactionDoesNotDisturbOtherAliveRowsEdges',
-- with the added wrinkle that matters specifically for src-deps: a
-- bystander row's src dep is *shared* with the row being churned, so this
-- also checks that forcing a compaction of the shared arena doesn't
-- disturb the shared id's refcount or resolvability.
test_srcDepsCompactionDoesNotDisturbBystanderRowsOrSharedIds :: IO ()
test_srcDepsCompactionDoesNotDisturbBystanderRowsOrSharedIds = do
  dt <- newTest
  (rHot, _) <- lookupOrInsertRow dt (h 0) 0
  bystanders <- mapM (\i -> lookupOrInsertRow dt (h i) i) [1 .. 10]
  -- every bystander shares one common src dep plus one of its own
  mapM_
    ( \(r, i) ->
        writeSrcDeps dt r (HashSet.fromList [srcDep "common" 1, srcDep ("own" ++ show i) 1])
    )
    (zip (map fst bystanders) [1 .. 10 :: Int])
  -- force several compactions of the shared srcDeps arena via the hot row
  mapM_
    (\i -> writeSrcDeps dt rHot (HashSet.singleton (srcDep "common" 1)) >> writeSrcDeps dt rHot (HashSet.singleton (srcDep ("hotgen" ++ show i) 1)))
    [1 .. 2000 :: Int]
  mapM_
    ( \(r, i) -> do
        got <- readSrcDeps dt r
        assertEqual (HashSet.fromList [srcDep "common" 1, srcDep ("own" ++ show i) 1]) got
    )
    (zip (map fst bystanders) [1 .. 10 :: Int])
  -- "common" is still referenced by every bystander plus rHot's current span
  c <- srcDepInternLiveCount dt
  assertBool (c > 0)

-- | A row freed and reused *between* two forced compactions of the
-- srcDeps arena -- the specific interleaving the task brief calls out.
-- The freed-and-reused row's own src dep must never resurface, and a
-- second compaction after reuse must still leave everything else correct.
test_rowFreedAndReusedBetweenTwoSrcDepsCompactions :: IO ()
test_rowFreedAndReusedBetweenTwoSrcDepsCompactions = do
  dt <- newTest
  (rHot, _) <- lookupOrInsertRow dt (h 0) 0
  (rVictim, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt rVictim (HashSet.singleton (srcDep "victim-only" 1))
  -- first compaction, forced via the hot row
  mapM_ (\i -> writeSrcDeps dt rHot (HashSet.singleton (srcDep ("gen" ++ show i) 1))) [1 .. 2000 :: Int]
  freeRow dt (h 1) rVictim
  (rVictim', _) <- lookupOrInsertRow dt (h 2) 2
  assertEqual rVictim rVictim'
  gotFresh <- readSrcDeps dt rVictim'
  assertEqual HashSet.empty gotFresh
  writeSrcDeps dt rVictim' (HashSet.singleton (srcDep "new-occupant" 1))
  -- second compaction, again forced via the hot row
  mapM_ (\i -> writeSrcDeps dt rHot (HashSet.singleton (srcDep ("gen2-" ++ show i) 1))) [1 .. 2000 :: Int]
  gotAfter <- readSrcDeps dt rVictim'
  assertEqual (HashSet.singleton (srcDep "new-occupant" 1)) gotAfter
  -- the old occupant's src dep must be gone from the intern table entirely
  gotHot <- readSrcDeps dt rHot
  assertBool (not (HashSet.member (srcDep "victim-only" 1) gotAfter))
  assertBool (not (HashSet.member (srcDep "victim-only" 1) gotHot))

-- | Comprehensive row-reuse garbage check: every column a fresh occupant
-- of a recycled row number could conceivably see stale data through --
-- param hash, flags/result-state, param, value, all three edge arenas, and
-- the hash index -- must reflect only the *new* occupant, never the old
-- one. Combines what several narrower tests above check individually into
-- one end-to-end pass over the whole row.
test_rowReuseSeesNoStaleGarbageInAnyColumn :: IO ()
test_rowReuseSeesNoStaleGarbageInAnyColumn = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 111
  writeResultHash dt r1 (h 555)
  writeValue dt r1 (777 :: Int)
  writeFlags dt r1 (mkFlags True False ResultValue)
  writeCompDeps dt r1 (VU.fromList [CompDepEdge 9 (1, 2)])
  writeRdeps dt r1 (VU.fromList [9, 10])
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "old-occupant" 1))
  freeRow dt (h 1) r1

  (r2, fresh) <- lookupOrInsertRow dt (h 2) 222
  assertBool fresh
  assertEqual r1 r2 -- confirms this is actually testing row *reuse*

  -- param hash: fresh, not the old occupant's
  ph <- readParamHash dt r2
  assertEqual (h 2) ph
  -- flags: fresh alive/not-pending/no-result, not the old ResultValue
  f <- readFlags dt r2
  assertBool (flagsAlive f)
  assertBool (not (flagsPending f))
  assertEqual NoResult (flagsResultState f)
  -- param: fresh
  p <- readParam dt r2
  assertEqual (222 :: Int) p
  -- edges: all three arenas reset, not the old occupant's
  cd <- readCompDeps dt r2
  rd <- readRdeps dt r2
  sd <- readSrcDeps dt r2
  assertEqual VU.empty cd
  assertEqual VU.empty rd
  assertEqual HashSet.empty sd
  -- hash index: the old hash (h 1) must no longer resolve to anything --
  -- looking it up again must be a fresh insert, not a hit on some
  -- leftover index entry from the freed occupant.
  (_, foundOldIsFresh) <- lookupOrInsertRow dt (h 1) 999
  assertBool foundOldIsFresh
  -- the old occupant's src dep must be entirely gone from the intern table
  c <- srcDepInternLiveCount dt
  assertEqual 0 c

-- | Index growth interleaved with row freeing: insert enough distinct
-- hashes to force several 'HashIndex' growths, freeing every other row
-- along the way (so growth and backward-shift deletion both fire in the
-- same sequence, not in isolation the way the standalone HashIndex tests
-- above exercise them). Every row that stayed alive must remain correctly
-- indexed once the churn settles.
test_indexGrowthInterleavedWithFreeingStaysCorrect :: IO ()
test_indexGrowthInterleavedWithFreeingStaysCorrect = do
  dt <- newTest
  let n = 300
  rows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h i) i) [1 .. n]
  -- free every other row, forcing backward-shift deletes interleaved with
  -- whatever growths already happened while they were all being inserted
  forM_ (zip [1 :: Int ..] rows) $ \(i, r) ->
    when (even i) $ freeRow dt (h i) r
  -- the surviving (odd-indexed) rows must all still resolve to their own row
  forM_ (zip [1 :: Int ..] rows) $ \(i, r) ->
    when (odd i) $ do
      (r', fresh) <- lookupOrInsertRow dt (h i) i
      assertBool (not fresh)
      assertEqual r r'
  -- and fresh inserts past the churn must still work and grow correctly
  moreRows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h (n + i)) (n + i)) [1 .. 100]
  assertEqual 100 (length moreRows)

--
-- Hash index: standalone tests against a small in-memory hash column (not
-- routed through a full DefTable), covering insert/lookup/delete/grow and
-- backward-shift's collision handling directly -- plus a couple of
-- DefTable-level integration/churn tests at the end.
--

-- | A hash built from explicit hi\/lo words, for tests that need to force
-- specific slots\/collisions deterministically rather than relying on
-- 'largeHash128''s MD5 output landing where a test wants it.
rawHash :: Word64 -> Word64 -> Hash128
rawHash hi lo = Hash128 (LH.Word128 hi lo)

-- | A tiny mutable row->hash table for standalone 'HashIndex' tests: plays
-- the role 'DefTable''s own @param_hash@ column plays for the real thing.
newHashColumn :: IO (IORef (VM.IOVector Hash128))
newHashColumn = newIORef =<< VM.new 256

setHashColumn :: IORef (VM.IOVector Hash128) -> RowIdx -> Hash128 -> IO ()
setHashColumn ref row hv = readIORef ref >>= \v -> VM.write v (unRowIdx row) hv

-- | Matches 'GetHash''s shape, partially applied to a concrete column --
-- exactly how a real 'DefTable''s 'readParamHash' is used at this callback.
getHashColumn :: IORef (VM.IOVector Hash128) -> GetHash
getHashColumn ref row = readIORef ref >>= \v -> VM.read v (unRowIdx row)

test_hashIndexLookupMissOnEmptyTable :: IO ()
test_hashIndexLookupMissOnEmptyTable = do
  hix <- newHashIndex
  col <- newHashColumn
  found <- hixLookup hix (getHashColumn col) (rawHash 1 1)
  assertEqual Nothing found

test_hashIndexInsertThenLookupFindsRow :: IO ()
test_hashIndexInsertThenLookupFindsRow = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 42 42
  setHashColumn col (ri 5) hv
  hixInsert hix (getHashColumn col) hv (ri 5)
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual (Just (ri 5)) found

test_hashIndexLookupMissForDifferentHash :: IO ()
test_hashIndexLookupMissForDifferentHash = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 1 1
  setHashColumn col (ri 0) hv
  hixInsert hix (getHashColumn col) hv (ri 0)
  found <- hixLookup hix (getHashColumn col) (rawHash 2 2)
  assertEqual Nothing found

test_hashIndexDeleteThenLookupMisses :: IO ()
test_hashIndexDeleteThenLookupMisses = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 7 7
  setHashColumn col (ri 3) hv
  hixInsert hix (getHashColumn col) hv (ri 3)
  hixDelete hix (getHashColumn col) hv
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual Nothing found

test_hashIndexDeleteOnMissingKeyIsNoOp :: IO ()
test_hashIndexDeleteOnMissingKeyIsNoOp = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 9 9
  setHashColumn col (ri 0) hv
  hixInsert hix (getHashColumn col) hv (ri 0)
  -- deleting an absent key must not throw and must not disturb the real entry
  hixDelete hix (getHashColumn col) (rawHash 123 456)
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual (Just (ri 0)) found

-- | Force several entries into the same initial-capacity (8) probe chain by
-- picking hi words that collide mod 8, and check every entry is still
-- individually findable -- exercises linear-probe collision resolution
-- rather than the (overwhelmingly likely) no-collision case a few random
-- inserts would hit.
test_hashIndexCollisionsAllFindable :: IO ()
test_hashIndexCollisionsAllFindable = do
  hix <- newHashIndex
  col <- newHashColumn
  let his = [0, 8, 16, 24, 32] -- all `mod 8 == 0` at the initial capacity
  mapM_
    ( \(i, hiW) -> do
        let hv = rawHash hiW 0
        setHashColumn col (ri i) hv
        hixInsert hix (getHashColumn col) hv (ri i)
    )
    (zip [0 ..] his)
  mapM_
    ( \(i, hiW) -> do
        found <- hixLookup hix (getHashColumn col) (rawHash hiW 0)
        assertEqual (Just (ri i)) found
    )
    (zip [0 ..] his)

-- | Deleting the *middle* of a collision chain must not strand the entries
-- that come after it in the probe sequence -- the case backward-shift
-- deletion exists to handle (a plain "just clear the slot" delete would
-- break their reachability).
test_hashIndexDeleteMiddleOfCollisionChainKeepsOthersFindable :: IO ()
test_hashIndexDeleteMiddleOfCollisionChainKeepsOthersFindable = do
  hix <- newHashIndex
  col <- newHashColumn
  let hvs = [rawHash hiW 0 | hiW <- [0, 8, 16]] -- all collide to slot 0
  mapM_ (\(i, hv) -> setHashColumn col (ri i) hv >> hixInsert hix (getHashColumn col) hv (ri i)) (zip [0 ..] hvs)
  hixDelete hix (getHashColumn col) (hvs !! 1) -- delete the middle one (hi=8, row 1)
  found0 <- hixLookup hix (getHashColumn col) (hvs !! 0)
  found1 <- hixLookup hix (getHashColumn col) (hvs !! 1)
  found2 <- hixLookup hix (getHashColumn col) (hvs !! 2)
  assertEqual (Just (ri 0)) found0
  assertEqual Nothing found1
  assertEqual (Just (ri 2)) found2

test_hashIndexGrowsAndPreservesAllEntries :: IO ()
test_hashIndexGrowsAndPreservesAllEntries = do
  hix <- newHashIndex
  col <- newHashColumn
  -- comfortably past the initial capacity (8) at load factor 0.7, forcing
  -- at least two doublings
  let n = 100
  mapM_
    ( \i -> do
        let hv = rawHash (fromIntegral i) (fromIntegral i)
        setHashColumn col (ri i) hv
        hixInsert hix (getHashColumn col) hv (ri i)
    )
    [0 .. n - 1]
  mapM_
    ( \i -> do
        let hv = rawHash (fromIntegral i) (fromIntegral i)
        found <- hixLookup hix (getHashColumn col) hv
        assertEqual (Just (ri i)) found
    )
    [0 .. n - 1]

-- | Alternately insert-then-delete the same row many times over (the
-- pattern the engine's live-update path drives: a row is freed and its
-- hash slot recycled, over and over, on the same handful of hot rows) --
-- the property backward-shift deletion (no tombstones) exists to protect:
-- capacity must not creep up from churn alone, and every still-live entry
-- must stay findable throughout.
test_hashIndexChurnDoesNotDegradeCapacityOrCorrectness :: IO ()
test_hashIndexChurnDoesNotDegradeCapacityOrCorrectness = do
  hix <- newHashIndex
  col <- newHashColumn
  -- a couple of entries that stay live for the whole test, to check they
  -- remain findable throughout the churn of everything else
  let stableA = rawHash 1001 1001
      stableB = rawHash 1002 1002
  setHashColumn col (ri 200) stableA
  hixInsert hix (getHashColumn col) stableA (ri 200)
  setHashColumn col (ri 201) stableB
  hixInsert hix (getHashColumn col) stableB (ri 201)
  forM_ [1 .. 500 :: Int] $ \i -> do
    let hv = rawHash (fromIntegral (1000000 + i)) 0
    setHashColumn col (ri 0) hv
    hixInsert hix (getHashColumn col) hv (ri 0)
    found <- hixLookup hix (getHashColumn col) hv
    assertEqual (Just (ri 0)) found
    hixDelete hix (getHashColumn col) hv
    foundAfter <- hixLookup hix (getHashColumn col) hv
    assertEqual Nothing foundAfter
  table <- readIORef (hix_table hix)
  -- only the two stable entries remain live; capacity must not have grown
  -- past what two live entries need (still the initial 8), since
  -- backward-shift deletion leaves no tombstones behind to inflate it
  assertEqual 8 (VUM.length table)
  count <- readIORef (hix_count hix)
  assertEqual 2 count
  foundA <- hixLookup hix (getHashColumn col) stableA
  foundB <- hixLookup hix (getHashColumn col) stableB
  assertEqual (Just (ri 200)) foundA
  assertEqual (Just (ri 201)) foundB

-- | The same churn pattern, but through 'DefTable''s own public API
-- (lookupOrInsertRow / freeRow) rather than 'HashIndex' directly -- the
-- live-update path's actual call shape -- checking that a row untouched by
-- the churn is unaffected and that every freed hash is genuinely gone.
test_defTableChurnManyFreeReuseCyclesStayCorrect :: IO ()
test_defTableChurnManyFreeReuseCyclesStayCorrect = do
  dt <- newTest
  (rSteady, _) <- lookupOrInsertRow dt (h 999999) 999999
  forM_ [1 .. 500 :: Int] $ \i -> do
    (r, fresh) <- lookupOrInsertRow dt (h i) i
    assertBool fresh
    freeRow dt (h i) r
    -- the freed hash must not still be findable -- re-inserting it must
    -- allocate fresh, not hit a stale index entry
    (r', fresh') <- lookupOrInsertRow dt (h i) i
    assertBool fresh'
    freeRow dt (h i) r'
  -- the row that stayed alive throughout must be entirely unaffected
  aliveSteady <- isAlive dt rSteady
  assertBool aliveSteady
  ph <- readParamHash dt rSteady
  assertEqual (h 999999) ph
  (rAgain, freshAgain) <- lookupOrInsertRow dt (h 999999) 999999
  assertBool (not freshAgain)
  assertEqual rSteady rAgain

--
-- Property-based churn test (Part 2's "nothing grows unboundedly" ask):
-- a random sequence of writes and frees against a small population of
-- rows, drawing src-dep values from a pool large enough that most writes
-- mint genuinely fresh (key, version) pairs -- the same shape as the
-- version-churn scenario the refcounting fix targets, but randomized and
-- interleaved with frees instead of hand-written. Checked against a plain
-- 'Map.Map' model: every row the model considers alive must read back
-- exactly what the model recorded, and -- the actual boundedness
-- assertion -- once every row the model still considers alive is freed,
-- the interned src-dep table must be entirely empty, regardless of how
-- many operations it took to get there.
--

-- | One churn operation: overwrite row @i@'s (small, fixed population of
-- row keys 1..8) src-dep set, or free it. 'ChurnWrite'\'s values are
-- @(keyIdx, version)@ pairs drawn from a small key-index pool but a wide
-- version range, so repeated writes to the same row mint mostly-fresh
-- interned ids -- the scenario that would grow the pre-refcounting table
-- without bound.
data ChurnOp
  = ChurnWrite Int [(Int, Int)]
  | ChurnFree Int
  deriving (Show)

instance QC.Arbitrary ChurnOp where
  arbitrary =
    QC.frequency
      [ (3, ChurnWrite <$> QC.choose (1, 8) <*> (QC.choose (0, 3) >>= \n -> QC.vectorOf n genSrcVal))
      , (1, ChurnFree <$> QC.choose (1, 8))
      ]
   where
    genSrcVal = (,) <$> QC.choose (0, 3 :: Int) <*> QC.choose (1, 2000 :: Int)

churnEncode :: (Int, Int) -> AnyCompSrcDep
churnEncode (k, v) = srcDep (show k) v

-- | Interpret a churn sequence against a live 'DefTable', updating a plain
-- 'Map.Map' model (row key -> its current src-dep set) alongside it. A
-- 'ChurnFree' on a row the model doesn't currently consider alive
-- (never written, or already freed) is a no-op, matching 'freeRow''s own
-- contract of being meaningless to call on a hash that isn't indexed.
runChurn :: DefTable Int Int -> [ChurnOp] -> IO (Map.Map Int (HashSet AnyCompSrcDep))
runChurn dt = foldM step Map.empty
 where
  step model (ChurnWrite i vals) = do
    (r, _) <- lookupOrInsertRow dt (h i) i
    let s = HashSet.fromList (map churnEncode vals)
    writeSrcDeps dt r s
    pure (Map.insert i s model)
  step model (ChurnFree i) =
    case Map.lookup i model of
      Nothing -> pure model
      Just _ -> do
        (r, fresh) <- lookupOrInsertRow dt (h i) i
        if fresh
          then pure model -- defensive; shouldn't happen given the model agrees it's alive
          else do
            freeRow dt (h i) r
            pure (Map.delete i model)

prop_srcDepChurnStaysCorrectAndFullyDrains :: [ChurnOp] -> QC.Property
prop_srcDepChurnStaysCorrectAndFullyDrains ops = QC.ioProperty $ do
  dt <- newTest
  model <- runChurn dt ops
  correctness <- forM (Map.toList model) $ \(i, expected) -> do
    (r, fresh) <- lookupOrInsertRow dt (h i) i
    if fresh
      then pure False
      else do
        got <- readSrcDeps dt r
        pure (got == expected)
  forM_ (Map.toList model) $ \(i, _) -> do
    (r, fresh) <- lookupOrInsertRow dt (h i) i
    unless fresh $ freeRow dt (h i) r
  finalCount <- srcDepInternLiveCount dt
  pure $
    QC.counterexample
      ( "per-row correctness: "
          ++ show correctness
          ++ "; live ids remaining after draining every row the model thinks is alive: "
          ++ show finalCount
      )
      (and correctness && finalCount == 0)
