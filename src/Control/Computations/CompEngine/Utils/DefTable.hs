{-# LANGUAGE AllowAmbiguousTypes #-}
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

 = Hash index: open addressing over the existing hash column

 The old per-def index was a persistent 'Data.HashMap.Strict.HashMap'
 'Hash128' 'Int' -- a HAMT leaf plus a boxed 'Hash128' key and boxed 'Int'
 value per entry, every one of them traced by every major GC, even though
 the key is pure duplication: every row's param hash already lives in the
 unboxed @param_hash@ column above. 'HashIndex' replaces it with a
 hand-rolled open-addressing table that stores only row ids
 ('Data.Int.Int32', @-1@ the empty-slot sentinel) in a flat
 'Data.Vector.Unboxed.Mutable.IOVector' -- no key storage, no boxing,
 nothing for the GC to trace.

 __Probe scheme.__ A candidate slot for hash @h@ is @w128_first h .\&.
 (capacity - 1)@ (capacity is always a power of two, so this is @mod@ via a
 mask) -- MD5-derived hashes (this codebase's only hash source, see
 "Utils/Hash.hs") are already uniformly distributed, so either 64-bit half
 works equally well as the slot seed; the high word was picked arbitrarily.
 Collisions resolve by linear probing (slot, slot+1, ... wrapping mod
 capacity). A probe candidate is verified by reading the *existing*
 @param_hash@ column for the row id stored at that slot and comparing the
 full 128-bit hash against it -- exactly the redundant-key trick this
 module is built around: the table never needs to store or compare against
 a second copy of the key.

 __Growth.__ Capacity starts at 8 and doubles whenever an insert would push
 the load factor (@(count+1) \/ capacity@) past 0.7; growing rebuilds a
 fresh table and reinserts every currently-occupied row id at its new slot
 (no key copies to move, since the table holds no keys -- only a row id and
 a lookup of that row's own @param_hash@ column entry).

 __Deletion: backward-shift, not tombstones.__ Rows are freed constantly on
 this engine's live-update path (recompute invalidates a row, a fresh param
 hash allocates a new one), so a tombstone strategy would accumulate dead
 markers under exactly the workload this table sees most -- degrading probe
 length over the life of the process with no reclamation short of a full
 rehash. Backward-shift deletion avoids that: removing an entry clears its
 slot, then walks forward relocating any subsequent entry whose ideal slot
 no longer cyclically requires the gap left behind (i.e. an entry a plain
 linear probe could no longer find past the new hole) back into the hole,
 opening a fresh hole where that entry used to sit, until a genuinely empty
 slot is reached -- the standard argument for why linear probing never
 needs tombstones. The net effect: repeated free/reuse churn (this module's
 own row-lifecycle contract, exercised by the live-update benchmark tens of
 thousands of times over) never grows the table's dead-slot fraction --
 capacity only ever grows in response to genuine live-entry growth, never
 in response to churn. See 'hixBackwardShift' for the algorithm (Knuth's
 backward-shift deletion for linear probing, adapted to not store keys).

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

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Bits
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.Int (Int32)
import qualified Data.LargeHashable as LH
import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, eqT)
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
-- Typed value columns (param / value): unboxed where the element type is
-- provably one of a fixed set of primitives, boxed otherwise -- see the
-- module haddock's "Typed value columns" section for the full rationale.
-- Selection happens once, at column-construction time, via 'eqT' against
-- @e@'s 'Typeable' instance; nothing on the read/write hot path branches on
-- the *type*, only on which data constructor the already-built 'Column'
-- happens to be (a single, cheap, wholly local pattern match GHC has every
-- opportunity to specialize per call site).
--

-- | A param or value column. @e@ is unconstrained on 'ColBoxed' -- every
-- type, including e.g. 'Data.ByteString.ByteString', goes through it; the
-- existential 'VUM.Unbox' dictionary plus the @e :~: u@ equality witness on
-- 'ColUnboxed' is what lets a single read\/write pair (below) work for both
-- branches without a second, type-directed dispatch at every access.
data Column e where
  ColBoxed :: !(IORef (VM.IOVector e)) -> Column e
  ColUnboxed :: (VUM.Unbox u) => !(e :~: u) -> !(IORef (VUM.IOVector u)) -> Column e

-- | Build a fresh, empty column for @e@, choosing 'ColUnboxed' when @e@ is
-- one of the fixed set of primitive types this module recognizes (mirrors
-- the task brief's list: Word32\/Word64\/Int\/Char\/Bool\/Double -- the
-- benchmark's own def bodies are @Word32@ param \/ @Word64@ result, so this
-- exercises the unboxed path directly) and 'ColBoxed' for everything else
-- (e.g. the test suite's @ByteString@/@String@ param\/result types).
-- Requires only 'Typeable' @e@ -- already implied by both
-- 'Control.Computations.CompEngine.Types.IsCompParam' and
-- 'Control.Computations.CompEngine.Types.IsCompResult', so this adds no
-- constraint beyond what every caller already has in scope; nothing in the
-- public engine interface (@defineComp@, @wireComp@, etc.) changes shape.
mkColumn :: forall e. (Typeable e) => IO (Column e)
mkColumn =
  case tryUnboxed of
    Just mk -> mk
    Nothing -> ColBoxed <$> (newIORef =<< VM.new 0)
 where
  tryUnboxed :: Maybe (IO (Column e))
  tryUnboxed =
    unboxedAs @Word32
      <|> unboxedAs @Word64
      <|> unboxedAs @Int
      <|> unboxedAs @Char
      <|> unboxedAs @Bool
      <|> unboxedAs @Double
  unboxedAs :: forall u. (Typeable u, VUM.Unbox u) => Maybe (IO (Column e))
  unboxedAs = case eqT :: Maybe (e :~: u) of
    Just Refl -> Just (ColUnboxed Refl <$> (newIORef =<< VUM.new 0))
    Nothing -> Nothing

-- | Test/introspection helper: which representation a column picked.
columnIsUnboxed :: Column e -> Bool
columnIsUnboxed ColBoxed{} = False
columnIsUnboxed ColUnboxed{} = True

colGrow :: Int -> Column e -> IO ()
colGrow needed (ColBoxed ref) = growBoxed ref needed
colGrow needed (ColUnboxed Refl ref) = growUnboxed ref needed

colRead :: Column e -> Int -> IO e
colRead (ColBoxed ref) row = readIORef ref >>= \v -> VM.read v row
colRead (ColUnboxed Refl ref) row = readIORef ref >>= \v -> VUM.read v row

colWrite :: Column e -> Int -> e -> IO ()
colWrite (ColBoxed ref) row e = readIORef ref >>= \v -> VM.write v row e
colWrite (ColUnboxed Refl ref) row e = readIORef ref >>= \v -> VUM.write v row e

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
-- Open-addressing hash->row index -- see the module haddock's "Hash index"
-- section for the full design rationale. Deliberately generic over how a
-- row's hash is fetched (a `getHash` callback) rather than hardwired to
-- `DefTable`'s own `dt_paramHash` column, so it can be built and tested in
-- isolation from the rest of the table.
--

-- | Sentinel marking an empty slot. Real row ids are always >= 0 (they come
-- from 'allocRow', a monotonic-or-recycled non-negative counter), so -1
-- can never collide with a genuine occupant.
hixEmpty :: Int32
hixEmpty = -1

-- | Starting capacity for a fresh index (a fresh 'DefTable' allocates one
-- of these per def, including many-small-defs, so this stays small).
hixInitialCap :: Int
hixInitialCap = 8

-- | Grow once occupancy would exceed this fraction of capacity.
hixLoadFactor :: Double
hixLoadFactor = 0.7

data HashIndex = HashIndex
  { hix_table :: !(IORef (VUM.IOVector Int32))
  , hix_count :: !(IORef Int)
  -- ^ number of occupied slots; tracked separately rather than derived by
  -- scanning, since backward-shift deletion (no tombstones) makes
  -- "occupied slot count" and "live entry count" the same thing at all
  -- times.
  }

newHashIndex :: IO HashIndex
newHashIndex = do
  v <- VUM.new hixInitialCap
  VUM.set v hixEmpty
  HashIndex <$> newIORef v <*> newIORef 0

-- | The slot a hash probes first. Capacity is always a power of two, so
-- @mod@ is a bitmask. Uses the hash's high word arbitrarily -- MD5 output
-- (this codebase's only hash source) is uniform in either half; see the
-- module haddock's "Hash index" section.
hixSlot :: Int -> Hash128 -> Int
hixSlot cap (Hash128 w) = fromIntegral (LH.w128_first w) .&. (cap - 1)
{-# INLINE hixSlot #-}

-- | Walk the probe sequence for @h@ against a concrete table, returning the
-- slot and row id of the matching entry (verified via @getHash@, not a
-- stored key -- there isn't one), or 'Nothing' once the sequence reaches an
-- empty slot. Shared by 'hixLookup' (wants the row) and 'hixDelete' (wants
-- the slot, to start the backward-shift walk from).
hixProbe :: VUM.IOVector Int32 -> (Int -> IO Hash128) -> Hash128 -> IO (Maybe (Int, Int))
hixProbe table getHash h = go start 0
 where
  cap = VUM.length table
  start = hixSlot cap h
  go i steps
    | steps >= cap = pure Nothing -- defensive: can't happen, load factor < 1 guarantees an empty slot
    | otherwise = do
        v <- VUM.read table i
        if v == hixEmpty
          then pure Nothing
          else do
            rh <- getHash (fromIntegral v)
            if rh == h
              then pure (Just (i, fromIntegral v))
              else go ((i + 1) .&. (cap - 1)) (steps + 1)

-- | Find the row currently indexed under @h@.
hixLookup :: HashIndex -> (Int -> IO Hash128) -> Hash128 -> IO (Maybe Int)
hixLookup hix getHash h = do
  table <- readIORef (hix_table hix)
  fmap snd <$> hixProbe table getHash h

-- | Insert @row@ under @h@. Caller's responsibility: @h@ is not already
-- present ('DefTable.lookupOrInsertRow' only calls this on a lookup miss).
-- Grows first if this insert would cross the load factor.
hixInsert :: HashIndex -> (Int -> IO Hash128) -> Hash128 -> Int -> IO ()
hixInsert hix getHash h row = do
  hixMaybeGrow hix getHash
  table <- readIORef (hix_table hix)
  let cap = VUM.length table
      go i = do
        v <- VUM.read table i
        if v == hixEmpty
          then VUM.write table i (fromIntegral row)
          else go ((i + 1) .&. (cap - 1))
  go (hixSlot cap h)
  modifyIORef' (hix_count hix) (+ 1)

hixMaybeGrow :: HashIndex -> (Int -> IO Hash128) -> IO ()
hixMaybeGrow hix getHash = do
  table <- readIORef (hix_table hix)
  count <- readIORef (hix_count hix)
  let cap = VUM.length table
  when (fromIntegral (count + 1) > hixLoadFactor * fromIntegral cap) $
    hixGrow hix getHash

-- | Double capacity and reinsert every currently-occupied row id at its new
-- slot. No keys to copy -- only row ids -- so this is a fresh table plus,
-- per occupied old slot, one @getHash@ call (the row's own @param_hash@
-- column entry) to find its new slot.
hixGrow :: HashIndex -> (Int -> IO Hash128) -> IO ()
hixGrow hix getHash = do
  old <- readIORef (hix_table hix)
  let oldCap = VUM.length old
      newCap = oldCap * 2
  new <- VUM.new newCap
  VUM.set new hixEmpty
  forM_ [0 .. oldCap - 1] $ \i -> do
    v <- VUM.read old i
    when (v /= hixEmpty) $ do
      rh <- getHash (fromIntegral v)
      insertFresh new newCap rh v
  writeIORef (hix_table hix) new
 where
  insertFresh table cap rh row = go (hixSlot cap rh)
   where
    go i = do
      w <- VUM.read table i
      if w == hixEmpty
        then VUM.write table i row
        else go ((i + 1) .&. (cap - 1))

-- | Remove the entry for @h@, then close the gap via backward-shift
-- deletion. No-op if @h@ isn't present (defensive; callers are expected to
-- only free a row they previously inserted).
hixDelete :: HashIndex -> (Int -> IO Hash128) -> Hash128 -> IO ()
hixDelete hix getHash h = do
  table <- readIORef (hix_table hix)
  found <- hixProbe table getHash h
  case found of
    Nothing -> pure ()
    Just (slot, _row) -> do
      let cap = VUM.length table
      VUM.write table slot hixEmpty
      modifyIORef' (hix_count hix) (subtract 1)
      hixBackwardShift table cap getHash slot

-- | Backward-shift deletion's gap-closing walk (Knuth's algorithm for
-- linear probing without tombstones, adapted to verify candidates via
-- @getHash@ rather than a stored key). @i@ starts at the position of the
-- hole just opened; @j@ walks forward from it. An entry at @j@ whose ideal
-- slot @k@ does /not/ cyclically fall in @(i, j]@ is one a plain linear
-- probe could no longer find past the hole at @i@, so it is relocated back
-- to @i@, opening a fresh hole at @j@; otherwise it is left alone and the
-- walk continues. Terminates the first time @j@ reaches a genuinely empty
-- slot.
hixBackwardShift :: VUM.IOVector Int32 -> Int -> (Int -> IO Hash128) -> Int -> IO ()
hixBackwardShift table cap getHash i0 = go i0 i0
 where
  go i jPrev = do
    let j = (jPrev + 1) .&. (cap - 1)
    vj <- VUM.read table j
    if vj == hixEmpty
      then pure ()
      else do
        hj <- getHash (fromIntegral vj)
        let k = hixSlot cap hj
            inRange = if i <= j then i < k && k <= j else i < k || k <= j
        if inRange
          then go i j -- entry at j stays reachable through the gap at i; keep scanning
          else do
            VUM.write table i vj
            VUM.write table j hixEmpty
            go j j -- the hole moved to j; continue scanning from there

--
-- The table
--

data DefTable p a = DefTable
  { dt_paramHash :: !(IORef (VUM.IOVector (Word64, Word64)))
  , dt_resultHash :: !(IORef (VUM.IOVector (Word64, Word64)))
  , dt_flags :: !(IORef (VUM.IOVector Word8))
  , dt_param :: !(Column p)
  , dt_value :: !(Column a)
  -- ^ valid only when 'flagsResultState' is 'ResultValue'. Unboxed when @a@
  -- is one of 'mkColumn''s recognized primitive types, boxed otherwise --
  -- see the "Typed value columns" section above.
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
  , dt_index :: !HashIndex
  -- ^ this def's own param-hash -> row index (its share of what used to be
  -- one global intern table). Open addressing over row ids only -- no
  -- stored key -- see the module haddock's "Hash index" section; probes
  -- verify against the row's own @param_hash@ column entry.
  , dt_free :: !(IORef [Int])
  , dt_len :: !(IORef Int)
  -- ^ logical row count (<= every column's current capacity)
  }

new :: forall p a. (Typeable p, Typeable a) => IO (DefTable p a)
new = do
  ph <- newIORef =<< VUM.new 0
  rh <- newIORef =<< VUM.new 0
  fl <- newIORef =<< VUM.new 0
  pa <- mkColumn
  va <- mkColumn
  cd <- newEdgeArena
  rd <- newEdgeArena
  sd <- newIORef =<< VM.new 0
  ix <- newHashIndex
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
  colGrow needed (dt_param dt)
  colGrow needed (dt_value dt)
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
  found <- hixLookup (dt_index dt) (readParamHash dt) h
  case found of
    Just row -> pure (row, False)
    Nothing -> do
      row <- allocRow dt
      writeHash (dt_paramHash dt) row h
      hixInsert (dt_index dt) (readParamHash dt) h row
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
  hixDelete (dt_index dt) (readParamHash dt) h
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
writeParam dt row p = colWrite (dt_param dt) row p

readParam :: DefTable p a -> Int -> IO p
readParam dt row = colRead (dt_param dt) row

readValue :: DefTable p a -> Int -> IO a
readValue dt row = colRead (dt_value dt) row

writeValue :: DefTable p a -> Int -> a -> IO ()
writeValue dt row a = colWrite (dt_value dt) row a

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

setHashColumn :: IORef (VM.IOVector Hash128) -> Int -> Hash128 -> IO ()
setHashColumn ref row hv = readIORef ref >>= \v -> VM.write v row hv

getHashColumn :: IORef (VM.IOVector Hash128) -> Int -> IO Hash128
getHashColumn ref row = readIORef ref >>= \v -> VM.read v row

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
  setHashColumn col 5 hv
  hixInsert hix (getHashColumn col) hv 5
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual (Just 5) found

test_hashIndexLookupMissForDifferentHash :: IO ()
test_hashIndexLookupMissForDifferentHash = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 1 1
  setHashColumn col 0 hv
  hixInsert hix (getHashColumn col) hv 0
  found <- hixLookup hix (getHashColumn col) (rawHash 2 2)
  assertEqual Nothing found

test_hashIndexDeleteThenLookupMisses :: IO ()
test_hashIndexDeleteThenLookupMisses = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 7 7
  setHashColumn col 3 hv
  hixInsert hix (getHashColumn col) hv 3
  hixDelete hix (getHashColumn col) hv
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual Nothing found

test_hashIndexDeleteOnMissingKeyIsNoOp :: IO ()
test_hashIndexDeleteOnMissingKeyIsNoOp = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 9 9
  setHashColumn col 0 hv
  hixInsert hix (getHashColumn col) hv 0
  -- deleting an absent key must not throw and must not disturb the real entry
  hixDelete hix (getHashColumn col) (rawHash 123 456)
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual (Just 0) found

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
        setHashColumn col i hv
        hixInsert hix (getHashColumn col) hv i
    )
    (zip [0 ..] his)
  mapM_
    ( \(i, hiW) -> do
        found <- hixLookup hix (getHashColumn col) (rawHash hiW 0)
        assertEqual (Just i) found
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
  mapM_ (\(i, hv) -> setHashColumn col i hv >> hixInsert hix (getHashColumn col) hv i) (zip [0 ..] hvs)
  hixDelete hix (getHashColumn col) (hvs !! 1) -- delete the middle one (hi=8, row 1)
  found0 <- hixLookup hix (getHashColumn col) (hvs !! 0)
  found1 <- hixLookup hix (getHashColumn col) (hvs !! 1)
  found2 <- hixLookup hix (getHashColumn col) (hvs !! 2)
  assertEqual (Just 0) found0
  assertEqual Nothing found1
  assertEqual (Just 2) found2

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
        setHashColumn col i hv
        hixInsert hix (getHashColumn col) hv i
    )
    [0 .. n - 1]
  mapM_
    ( \i -> do
        let hv = rawHash (fromIntegral i) (fromIntegral i)
        found <- hixLookup hix (getHashColumn col) hv
        assertEqual (Just i) found
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
  setHashColumn col 200 stableA
  hixInsert hix (getHashColumn col) stableA 200
  setHashColumn col 201 stableB
  hixInsert hix (getHashColumn col) stableB 201
  forM_ [1 .. 500 :: Int] $ \i -> do
    let hv = rawHash (fromIntegral (1000000 + i)) 0
    setHashColumn col 0 hv
    hixInsert hix (getHashColumn col) hv 0
    found <- hixLookup hix (getHashColumn col) hv
    assertEqual (Just 0) found
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
  assertEqual (Just 200) foundA
  assertEqual (Just 201) foundB

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
