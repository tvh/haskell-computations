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
 for the @srcDeps@ column). A boxed column's freshly-grown capacity is left
 as the @vector@ package's own "uninitialised element" error thunk (see
 'growBoxed') -- exactly the loud-failure-on-premature-read canary this
 module established for row lifecycle in an earlier increment (dead ids
 there, dead cells here), now extended to row storage. Growth for the
 @flags@ column (and, see below, the edge-arena @len@ columns) is the one
 exception: it is explicitly zeroed (not left as unboxed garbage), because a
 stray nonzero byte there would silently read as an occupied row or a
 nonempty edge span.

 = Edge storage: per-def CSR arenas, not per-row vectors

 An earlier increment of this rewrite gave every row its own small unboxed
 'VU.Vector' for @compDeps@/@rdeps@ -- correct, but each such vector is its
 own heap object (a boxed cell in a 'VM.Mutable' column holding a 'VU.Vector'
 wrapper plus its own 'Data.Array.Byte.ByteArray#'), so a fan-in-3 row paid
 for several small, separately-traced nursery allocations to hold ~24-72
 bytes of actual edge payload. This module instead gives each def two
 shared, growable, flat unboxed 'Word64' arenas (one for @compDeps@, one for
 @rdeps@) plus two unboxed per-row columns (@offset@ :: 'Int32', @len@ ::
 'Word32') indexing into them -- CSR (compressed sparse row), with the
 arena itself, not each row, as the unit of (de)allocation. A comp-dep edge
 is 3 words (target 'DefRef', observed-hash hi, observed-hash lo, matching
 exactly what the old per-row @VU.Vector (Int, Word64, Word64)@ carried --
 see "Preserved semantics" below); an rdep edge is 1 word (target
 'DefRef'). One large arena is the point of this exercise: unlike a swarm of
 small per-row vectors, a large flat array is a single object GHC's
 allocator places in the large-object area once it crosses the pinned/large
 threshold, where the copying collector never traces or copies it -- see the
 module's "Small defs" note below for the one case where this doesn't apply.

 __Mutation strategy: append-new-span, mark-old-span-dead, compact on a
 dead-fraction threshold.__ Chosen over "CSR with per-row slack" because
 every write here (@writeCompDeps@/@writeRdeps@, both called from
 "SimpleStateIf.hs") replaces a row's /entire/ edge set at once -- there is
 no in-place single-edge splice anywhere on the hot path for slack to help
 with, only whole-row replacement, so append-only is both simpler and no
 less dense. This is the same append-only-arena shape the Rust port's Stage
 5 uses for /param/ bytes (@persistence-benchmark-notes.md@, "Stage 5",
 @param_off@/@param_len@ indexing into one @param_arena@ per def), applied
 here to edges instead: a write appends the new span at the arena's current
 end and bumps a per-def dead-word counter by the row's /previous/ span
 length (if any); nothing is physically overwritten in place, so a stale
 span is simply orphaned -- exactly like 'freeRow' already leaves a freed
 row's other columns as untouched garbage, gated by flags. Unlike the Rust
 param arena (deliberately never compacted -- params are small, so even
 permanently-unreclaimed spans are cheap), edges here are compacted,
 because this task's whole point is bounding the arena: repeated
 recomputation of the /same/ live rows (exactly what the benchmark's live-
 update phase does, tens of thousands of times over the same 999,760-row
 graph) would otherwise make the arena grow without bound, one orphaned
 span per rerun, forever.

 Compaction ('eaCompact') rebuilds a def's arena from scratch, walking every
 row 0..rowCount-1 and keeping only the /current/ span of every row that is
 currently alive; a row that isn't alive contributes nothing, whether or
 not it was ever given an explicit new write after going dead -- this is
 what lets a freed-but-not-yet-reused row's orphaned span get reclaimed
 too, not just rows some other write already knew were stale. This is why
 "GC" (the row-lifecycle kind, 'freeRow') is the natural trigger the task
 brief asks for: freeing a row doesn't touch its edge columns at all (see
 "Row lifecycle and reuse" below), so the /existing/ garbage-tolerance
 design already guarantees a dead row's stale span is inert and safe to
 discard whenever compaction next runs, without freeRow itself needing to
 know anything about edge arenas. 'maybeCompact' fires this whenever, after
 an append, a def's dead-word count exceeds half its used length (and the
 arena is past a minimum size not worth an O(rowCount) scan for) --
 the standard amortized array-with-tombstones argument: a compaction
 immediately halves (at least) the dead fraction, so the arena's peak size
 is bounded to roughly 2x its live data, and the total compaction work
 across N writes is O(N) amortized, tied to the write path itself rather
 than a separately scheduled sweep.

 __Small defs.__ A def with few rows (or few edges per row) never
 accumulates enough words to reach GHC's large-object threshold (currently
 a few kB), so its arena is a small nursery-resident array like everything
 else -- this module does not special-case that, and it's fine: the whole
 argument for "large arrays escape the nursery" only pays off where it's
 needed, on the defs with many rows (this benchmark's front-loaded levels,
 up to 205,000 rows), which is also exactly where the old per-row-vector
 scheme paid the worst multiplicative overhead. A small def's arena stays
 small precisely because its total edge payload is small; there is nothing
 to reclaim by treating it differently.

 __Preserved semantics.__ Per-edge observed-result-hash tracking on comp-dep
 edges is unchanged bit-for-bit from the prior representation: each edge is
 still (target 'DefRef', observed hash hi, observed hash lo), and
 @test_modifcationWhileWorkingOnQueue@'s impure-cap detection (which
 compares a row's previously-recorded per-edge observed version against
 what it observes on the current run) depends on exactly this, per
 "SimpleStateIf.hs"'s module haddock. Only the storage layout changed; nothing
 was dropped to save bytes.

 = Row lifecycle and reuse

 A freed row is pushed onto a free list and its 'flags' cleared (not-alive,
 no result, not pending); every other column is left untouched -- stale
 param/value/edges from the previous occupant, including its edge arena
 span, which becomes reclaimable garbage handled by the edge-arena
 compaction machinery above rather than by 'freeRow' itself. This is safe
 under the invariant that every read of a possibly-stale column is gated
 behind a flags check (@alive@ for identity-ish columns, @hasResult@ for
 the value/result-hash columns) that is unconditionally cleared before the
 row can be reused, so the garbage can never be observed. Reuse does not
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

import Control.Monad (forM_, when)
import Data.Bits
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.Int (Int32)
import qualified Data.LargeHashable as LH
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word32, Word64, Word8)
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

-- | Grow an unboxed column so it has room for at least @needed@ elements.
-- Freshly grown capacity is unspecified garbage bytes -- safe only because
-- every unboxed column grown this way is read strictly behind a flags (or
-- edge-length) check that a legitimate write establishes first.
growUnboxed :: VUM.Unbox e => IORef (VUM.IOVector e) -> Int -> IO ()
growUnboxed ref needed = do
  vec <- readIORef ref
  let cap = GM.length vec
  when (needed > cap) $ do
    vec' <- GM.unsafeGrow vec (max (needed - cap) (max 4 cap))
    writeIORef ref vec'

-- | Like 'growUnboxed', but explicitly zeroes the freshly grown region.
-- Used for @flags@ (a stray nonzero garbage byte there would silently read
-- as an occupied row) and for an edge arena's @len@ column (a stray
-- nonzero garbage value there would silently read as a nonempty edge
-- span) -- both are the "is this row's data safe to read" gate for their
-- respective columns, unlike every other unboxed column here, which is
-- read only after one of these two checks has already gated it.
growUnboxedZeroed :: (VUM.Unbox e, Num e) => IORef (VUM.IOVector e) -> Int -> IO ()
growUnboxedZeroed ref needed = do
  vec <- readIORef ref
  let cap = GM.length vec
  when (needed > cap) $ do
    let extra = max (needed - cap) (max 4 cap)
    vec' <- GM.unsafeGrow vec extra
    GM.set (GM.unsafeSlice cap extra vec') 0
    writeIORef ref vec'

--
-- CSR-style per-def edge arenas -- see the module haddock's "Edge storage"
-- section for the full design rationale. Everything below is stride-
-- generic (stride 3 for compDeps, 1 for rdeps) and knows nothing about
-- 'DefTable' or 'DefRef'; the typed (un)flattening into
-- @VU.Vector (Int, Word64, Word64)@ / @VU.Vector Int@ happens at the
-- DefTable column-access boundary further down.
--

data EdgeArena = EdgeArena
  { ea_off :: !(IORef (VUM.IOVector Int32))
  -- ^ per-row arena word-offset of the row's current span
  , ea_len :: !(IORef (VUM.IOVector Word32))
  -- ^ per-row edge count (0 => no span, @ea_off@ is meaningless)
  , ea_data :: !(IORef (VUM.IOVector Word64))
  -- ^ the shared, growable, append-only arena itself
  , ea_used :: !(IORef Int)
  -- ^ arena append point, in words
  , ea_dead :: !(IORef Int)
  -- ^ words within [0, ea_used) known to belong to an orphaned span
  }

newEdgeArena :: IO EdgeArena
newEdgeArena =
  EdgeArena
    <$> (newIORef =<< VUM.new 0)
    <*> (newIORef =<< VUM.new 0)
    <*> (newIORef =<< VUM.new 0)
    <*> newIORef 0
    <*> newIORef 0

-- | Grow an edge arena's per-row @offset@/@len@ columns to at least
-- @needed@ rows. Does not touch the shared word arena itself -- that grows
-- independently, on append, in 'eaWrite'.
eaGrowRows :: EdgeArena -> Int -> IO ()
eaGrowRows ea needed = do
  growUnboxed (ea_off ea) needed
  growUnboxedZeroed (ea_len ea) needed

-- | Only compact once an arena has reached this many words -- below this,
-- the O(rowCount) scan a compaction costs isn't worth it.
compactMinWords :: Int
compactMinWords = 4096

-- | Compact iff the def's dead words are more than half of its used
-- words (and the arena is large enough to bother) -- see the module
-- haddock's amortized-cost argument.
maybeCompact :: EdgeArena -> (Int -> IO Bool) -> Int -> Int -> IO ()
maybeCompact ea isRowAlive rows stride = do
  used <- readIORef (ea_used ea)
  dead <- readIORef (ea_dead ea)
  when (used >= compactMinWords && dead * 2 > used) $
    eaCompact ea isRowAlive rows stride

-- | Rebuild the arena keeping only the current span of every currently-
-- alive row (in row order); every other row (dead, or alive with no
-- edges) has its offset/len columns zeroed. A dead row contributes nothing
-- regardless of whether it was ever given an explicit new write after
-- going dead -- see the module haddock for why that's the key property
-- that makes tying this to the write path (rather than row-free) correct.
eaCompact :: EdgeArena -> (Int -> IO Bool) -> Int -> Int -> IO ()
eaCompact ea isRowAlive rows stride = do
  used <- readIORef (ea_used ea)
  srcV <- readIORef (ea_data ea)
  dstV <- VUM.new (max 4 used)
  offV <- readIORef (ea_off ea)
  lenV <- readIORef (ea_len ea)
  writeIdxRef <- newIORef 0
  forM_ [0 .. rows - 1] $ \row -> do
    alive <- isRowAlive row
    len <- VUM.read lenV row
    if alive && len > 0
      then do
        off <- VUM.read offV row
        writeIdx <- readIORef writeIdxRef
        let n = fromIntegral len * stride
        GM.unsafeCopy (VUM.slice writeIdx n dstV) (VUM.slice (fromIntegral off) n srcV)
        VUM.write offV row (fromIntegral writeIdx)
        writeIORef writeIdxRef (writeIdx + n)
      else do
        VUM.write offV row 0
        VUM.write lenV row 0
  finalUsed <- readIORef writeIdxRef
  writeIORef (ea_data ea) dstV
  writeIORef (ea_used ea) finalUsed
  writeIORef (ea_dead ea) 0

-- | Read a row's edge span as a flat, stride-major 'VU.Vector Word64' (a
-- safe copy -- the arena keeps mutating after this call returns). Empty
-- for a row with no edges.
eaRead :: EdgeArena -> Int -> Int -> IO (VU.Vector Word64)
eaRead ea row stride = do
  lenV <- readIORef (ea_len ea)
  len <- VUM.read lenV row
  if len == 0
    then pure VU.empty
    else do
      offV <- readIORef (ea_off ea)
      off <- VUM.read offV row
      dataV <- readIORef (ea_data ea)
      VU.freeze (VUM.slice (fromIntegral off) (fromIntegral len * stride) dataV)

-- | Overwrite a row's entire edge span with @flat@ (already stride-major
-- flattened; its length must be a multiple of @stride@). An empty write
-- just zeroes the row's offset/len -- clearing a row's edges never
-- appends to (or grows) the arena. Otherwise appends a fresh span at the
-- arena's current end, marks the row's previous span (if any) as dead
-- weight, then lets 'maybeCompact' decide whether the def's dead fraction
-- now warrants reclaiming it.
eaWrite :: EdgeArena -> Int -> (Int -> IO Bool) -> Int -> Int -> VU.Vector Word64 -> IO ()
eaWrite ea row isRowAlive rows stride flat = do
  lenV <- readIORef (ea_len ea)
  offV <- readIORef (ea_off ea)
  oldLen <- VUM.read lenV row
  when (oldLen > 0) $
    modifyIORef' (ea_dead ea) (+ (fromIntegral oldLen * stride))
  let numWords = VU.length flat
  if numWords == 0
    then do
      VUM.write offV row 0
      VUM.write lenV row 0
    else do
      used <- readIORef (ea_used ea)
      growUnboxed (ea_data ea) (used + numWords)
      dataV <- readIORef (ea_data ea)
      VU.unsafeCopy (VUM.slice used numWords dataV) flat
      writeIORef (ea_used ea) (used + numWords)
      VUM.write offV row (fromIntegral used)
      VUM.write lenV row (fromIntegral (numWords `div` stride))
  maybeCompact ea isRowAlive rows stride

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
  , dt_compDeps :: !EdgeArena
  -- ^ flat forward comp-dep edges, stride 3: (packed 'DefRef' this row
  -- depends on, the target's result hash *as observed* when this row last
  -- ran, split hi/lo) -- needed to replicate the old VerList-based "impure
  -- cap" detection, which must distinguish "I depend on the same target at
  -- the same version as before, yet produced a different result"
  -- (genuinely impure) from "I depend on the same target but at a newly-
  -- changed version" (an ordinary, expected recompute) -- a target-set
  -- comparison alone can't tell those apart. A target with no result at
  -- observation time (a failed dependency) is encoded as the sentinel
  -- @(maxBound, maxBound)@; colliding with a real MD5-derived hash is not
  -- a realistic concern. See the module haddock's "Edge storage" section
  -- for the CSR-arena layout this lives in.
  , dt_rdeps :: !EdgeArena
  -- ^ flat reverse comp-dep edges (packed 'DefRef's depending on this
  -- row), stride 1.
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
  cd <- newEdgeArena
  rd <- newEdgeArena
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
  eaGrowRows (dt_compDeps dt) needed
  eaGrowRows (dt_rdeps dt) needed
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
-- (cheap -- an empty edge write never touches the arena, see 'eaWrite' --
-- not per-row garbage, an explicit reset, since a freshly *allocated* row
-- -- as opposed to a *reused* one -- has never had edges and there is
-- nothing stale to gate), and returns 'True' as the second component.
-- Returns 'False' on a hit.
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

-- | Flatten a comp-dep edge set into stride-3 words (target, hash-hi,
-- hash-lo) for 'eaWrite'.
flattenCompDeps :: VU.Vector (Int, Word64, Word64) -> VU.Vector Word64
flattenCompDeps xs = VU.generate (3 * VU.length xs) go
 where
  go i = case i `divMod` 3 of
    (n, 0) -> let (t, _, _) = xs VU.! n in fromIntegral t
    (n, 1) -> let (_, hi, _) = xs VU.! n in hi
    (n, _) -> let (_, _, lo) = xs VU.! n in lo

-- | Inverse of 'flattenCompDeps'.
unflattenCompDeps :: VU.Vector Word64 -> VU.Vector (Int, Word64, Word64)
unflattenCompDeps flat = VU.generate (VU.length flat `div` 3) go
 where
  go n = (fromIntegral (flat VU.! (3 * n)), flat VU.! (3 * n + 1), flat VU.! (3 * n + 2))

readCompDeps :: DefTable p a -> Int -> IO (VU.Vector (Int, Word64, Word64))
readCompDeps dt row = unflattenCompDeps <$> eaRead (dt_compDeps dt) row 3

writeCompDeps :: DefTable p a -> Int -> VU.Vector (Int, Word64, Word64) -> IO ()
writeCompDeps dt row xs = do
  n <- rowCount dt
  eaWrite (dt_compDeps dt) row (isAlive dt) n 3 (flattenCompDeps xs)

-- | Just the target refs of a comp-dep edge set, discarding the observed
-- version -- what the rdeps graph (add/remove edges, GC liveness) cares
-- about.
compDepTargets :: VU.Vector (Int, Word64, Word64) -> VU.Vector Int
compDepTargets = VU.map (\(r, _, _) -> r)

readRdeps :: DefTable p a -> Int -> IO (VU.Vector Int)
readRdeps dt row = VU.map fromIntegral <$> eaRead (dt_rdeps dt) row 1

writeRdeps :: DefTable p a -> Int -> VU.Vector Int -> IO ()
writeRdeps dt row xs = do
  n <- rowCount dt
  eaWrite (dt_rdeps dt) row (isAlive dt) n 1 (VU.map fromIntegral xs)

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
        let cd = VU.fromList [(i, fromIntegral i, fromIntegral (i + 1))]
        writeCompDeps dt r cd
        let rd = VU.fromList [i, i + 1, i + 2]
        writeRdeps dt r rd
    )
    [1 .. 3000]
  gotCd <- readCompDeps dt r
  assertEqual (VU.fromList [(3000, 3000, 3001)]) gotCd
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
        writeCompDeps dt r (VU.fromList [(100 + i, fromIntegral i, fromIntegral i)])
        writeRdeps dt r (VU.fromList [200 + i])
    )
    (zip (map fst bystanders) [1 .. 10])
  mapM_
    ( \i -> writeCompDeps dt rHot (VU.fromList [(i, fromIntegral i, fromIntegral i)])
    )
    [1 .. 3000 :: Int]
  mapM_
    ( \(r, i) -> do
        cd <- readCompDeps dt r
        assertEqual (VU.fromList [(100 + i, fromIntegral i, fromIntegral i)]) cd
        rd <- readRdeps dt r
        assertEqual (VU.fromList [200 + i]) rd
    )
    (zip (map fst bystanders) [1 .. 10])

test_freedRowsDontResurfaceAfterCompaction :: IO ()
test_freedRowsDontResurfaceAfterCompaction = do
  dt <- newTest
  (rKeep, _) <- lookupOrInsertRow dt (h 0) 0
  writeCompDeps dt rKeep (VU.fromList [(999, 1, 1)])
  (rDoomed, _) <- lookupOrInsertRow dt (h 1) 1
  writeCompDeps dt rDoomed (VU.fromList [(888, 2, 2)])
  freeRow dt (h 1) rDoomed
  -- force compaction via repeated overwrites of the surviving row
  mapM_
    (\i -> writeCompDeps dt rKeep (VU.fromList [(i, fromIntegral i, fromIntegral i)]))
    [1 .. 3000 :: Int]
  aliveKeep <- isAlive dt rKeep
  assertBool aliveKeep
  cd <- readCompDeps dt rKeep
  assertEqual (VU.fromList [(3000, 3000, 3000)]) cd
  aliveDoomed <- isAlive dt rDoomed
  assertBool (not aliveDoomed)
