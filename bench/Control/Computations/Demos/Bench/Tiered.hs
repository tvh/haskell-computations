{-# LANGUAGE ApplicativeDo #-}

{- | A third benchmark, alongside "Control.Computations.Demos.Bench.Main" and
 "Control.Computations.Demos.Bench.Hospital", built to measure two things
 neither of those two can:

 __1. What dispatch-then-drain costs when a batch genuinely mixes source and
 eval leaves.__ The engine's batch machinery
 ("Control.Computations.CompEngine.Impl"'s @doSuspended@, @CompReqCombined@
 case) partitions a batch's leaves into dispatchable source jobs and
 engine-thread work, and always joins every source worker before any
 engine-thread work begins -- dispatch, then drain, never overlapped (see
 that module's own three-reason comment for why). Hospital's graph is almost
 entirely stratified -- leaf comps read sources, interior comps read only
 other comps -- so only one comp in that whole graph
 ('Control.Computations.Demos.Bench.Hospital.patientSummaryComp') ever mixes
 both leaf kinds in one batch, at one instance per patient. This module gives
 four more comps, at four different depths, a source read alongside their
 existing comp dependencies (see \"Mixed dependencies\" below), so dispatch-
 then-drain's cost becomes visible at the scale of the whole graph instead of
 one batch in roughly a thousand.

 __2. Whether heterogeneous per-source latency re-ranks concurrency against
 bundling.__ Hospital's single @HOSPITAL_BENCH_SRC_LATENCY_US@ knob gives
 every source the same latency, so every batch is equally worth optimising --
 which is exactly the shape Stage 5's width x latency grid measured bundling
 winning under (2.5x). A real system never looks like that: some
 dependencies are in-memory caches, some are slow downstream services. With a
 slow source among fast ones, a batch's wall time is bounded by its slowest
 leaf, not its average -- which should make /concurrency across instances/
 (overlapping several different sources' round trips) matter more than
 /bundling within one/ (collapsing several same-instance keys into one round
 trip), inverting the ranking Stage 5 measured. See \"Per-source latency
 tiers\" below and 'docs/benchmark-notes.md'\'s new stage for whether that
 prediction actually holds, and why.

 __Shape preserved from Hospital, deliberately__: same comps, same
 per-patient counts (250 vitals, 50 windows, 180 labs, 18 meds, 153
 interactions, 140 notes -- see
 'Control.Computations.Demos.Bench.Hospital.depthHistogram'\'s haddock for
 why that sums to 976), same depth distribution. Everything below is
 additive -- extra source reads folded into existing applicative batches,
 never a restructured graph -- so any measured difference between this
 benchmark and Hospital's own (latency-0, uniform-latency) numbers is
 attributable to the two changes above, not to comparing different shapes.
 The packing tricks, key-fallback scheme, and depth-histogram derivation are
 copied from that module rather than imported: 'Hospital.hs' exports only
 'Control.Computations.Demos.Bench.Hospital.hospitalBenchMain' (everything
 else is internal to keep that module a stable, unchanging no-regression
 guard -- see its own module haddock), so a sibling benchmark that needs the
 same graph-shape machinery has no third option besides duplicating it, the
 same call that module's own driver/'getRssMb' duplication from
 "Control.Computations.Demos.Bench.Main" already made.

 = Mixed dependencies

 Four comps, one per depth band, each gain a source read folded into their
 existing applicative batch via @ApplicativeDo@ (never @mapM_@\/@forM_@ --
 see the module haddock this note echoes in "Hospital.hs" for why that
 distinction is what makes a batch real):

 +-------------------------+------------------------------+-----------+
 | comp                    | reads                        | from      |
 +=========================+==============================+===========+
 | 'vitalWindowComp' (p,w) | the monitor's calibration    | vitals    |
 +-------------------------+------------------------------+-----------+
 | 'interactionComp' (p,i) | the drug-pair interaction row| pharmacy  |
 +-------------------------+------------------------------+-----------+
 | 'riskScoreComp' p       | risk-model coefficients      | labs      |
 +-------------------------+------------------------------+-----------+
 | 'wardCensusComp' w      | the ward's bed capacity      | adt       |
 +-------------------------+------------------------------+-----------+

 __NOTE: 'riskModelKey' is one single key shared by the whole run__ -- every
 patient's 'riskScoreComp' instance reads the exact same key, deliberately.
 Nearly every key Hospital (and this module's other three new reads) ever
 touches has exactly one dependent, which is why Stage 7's 'SrcKeyArena'
 small-size optimisation
 ("Control.Computations.CompEngine.Utils.SrcIndex") paid off: the
 @SrcKeyOne@ representation, no vector at all. One key with a dependent per
 patient (hundreds to a thousand, depending on scale) exercises the
 @SrcKeyMany@ path instead -- the one Hospital's own graph almost never
 reaches -- and gives a realistic invalidation fan-out: changing this one key
 should restale a large fraction of the graph in one propagation round,
 something no other key in either benchmark can demonstrate.

 = Per-source latency tiers

 "Control.Computations.Demos.Bench.SystemSrc" already carries a per-instance
 latency field (added for Hospital); this module is the first caller to
 actually give each of the five instances its own rather than sharing one
 uniform @_SRC_LATENCY_US@ knob. Base tiers (roughly a 40x spread, 1 ms as
 the ceiling -- see 'baseLatencyUsFor'):

 +----------+----------------+
 | source   | base latency   |
 +==========+================+
 | vitals   | 25 microseconds  |
 +----------+----------------+
 | notes    | 100 microseconds |
 +----------+----------------+
 | labs     | 250 microseconds |
 +----------+----------------+
 | adt      | 500 microseconds |
 +----------+----------------+
 | pharmacy | 1000 microseconds |
 +----------+----------------+

 Scaled by a single multiplier, @TIERED_BENCH_LATENCY_MULT@ (default @1.0@,
 giving the table above exactly; @0@ disables every simulated delay, for a
 cheap full-scale structural run -- instance counts, depth histogram,
 memory -- with no wall-clock cost from latency at all).

 __These tiers are real, not nominal.__ 'Control.Computations.Demos.Bench.SystemSrc'
 simulates each round trip with a __safe-FFI @usleep@ call__, not
 'Control.Concurrent.threadDelay' -- see that module's @c_usleep@ haddock for
 the full story: @threadDelay@ was tried first and rejected, because on the
 measurement machine it has an effective floor around 1.28 ms that collapsed
 every tier in the table above to roughly the same real delay (a standalone
 microbenchmark requesting 25, 100, 250, 500 and 1000 microseconds each
 landed within 1.13-1.34 ms actual). @usleep@ recovers a real, proportional
 spread down to tens of microseconds -- but it must be called via a @safe@,
 not @unsafe@, foreign import: an @unsafe@ call does not release its calling
 capability for the call's duration, which would silently serialize
 concurrent source calls against each other and zero out the very
 concurrency effect this benchmark exists to measure, with no visible error.
 @usleep@ is still not exact -- roughly __+25% overhead__ versus the
 requested delay, with an additive floor near __20 microseconds__ -- so the
 40x spread the table above specifies is realized proportionally, not to the
 microsecond. See 'docs/benchmark-notes.md's new stage for the measured
 comparison table (@threadDelay@ vs. safe-FFI @usleep@ vs. busy-wait, both
 for per-call accuracy and for whether concurrent calls still overlap).

 __Deterministic per-key jitter__, off by default and behind its own knob
 (@TIERED_BENCH_JITTER=1@): see
 "Control.Computations.Demos.Bench.SystemSrc"'s 'Control.Computations.Demos.Bench.SystemSrc.jitterUs'
 haddock. Derived from a hash of the key, never a random-number generator,
 so a jittered run stays exactly as reproducible as an unjittered one
 (same reruns, same allocation) -- only wall-clock timing gains a spread.
 Tail latency (p99 vs p50) is where concurrency earns its keep; a constant
 delay has nothing tail-shaped to hide behind, which is exactly why Hospital
 (uniform latency, no jitter) could never show this and this module can.

 = The bundling knob

 @TIERED_BENCH_BUNDLING@ (default @1@, on) toggles
 "Control.Computations.Demos.Bench.SystemSrc"'s runtime batching override
 for every one of the five instances at once. This is what turns \"does
 concurrency across instances beat bundling within one\" from a confounded
 question into a real 2x4 matrix: with bundling forced off, every
 same-instance multi-key request pays its per-key latency independently
 (falling back to 'Control.Computations.CompEngine.CompSrc.CompSrc'\'s own
 default sequential 'Control.Computations.CompEngine.CompSrc.compSrcExecuteBatch'),
 so bundling's own contribution can be measured in isolation from
 concurrency width, and vice versa -- see 'docs/benchmark-notes.md's new
 stage for the resulting matrix and what it does and doesn't show.

 = Scale

 Mixed dependencies raise the per-patient source-call count above Hospital's
 own ~1,612 reads\/~590 batches (\@vitalWindowComp\@ +1 per instance x50,
 \@interactionComp\@ +1 x153, \@riskScoreComp\@ +1 x1 -- ~1,816\/patient
 before wardCensusComp's per-ward, not per-patient, addition), to ~1,935
 *batch calls*\/patient once bundling is accounted for (every one of
 'interactionComp'\'s 153 pharmacy reads is its own nested, sequential cap
 evaluation -- see the module haddock's "Per-source latency tiers" note --
 so none of them bundle with each other; only same-comp multi-key reads
 like 'vitalComp'\'s 3 vitals keys collapse to one call).

 With real, proportional tiers (see "Per-source latency tiers" above -- not
 the flat ~1.28 ms every batch call would have cost under 'threadDelay'),
 wall time is no longer proportional to raw batch-call count alone; it is
 dominated by whichever source pays the most total simulated latency,
 batch-call count times its own tier. Most calls land on the cheap
 vitals\/notes\/labs tiers; the exception is 'interactionComp'\'s 153
 pharmacy reads per patient, which (per the note above) never bundle with
 each other, so pharmacy pays close to its full 1 ms tier on nearly every
 one of those calls -- pharmacy dominates this benchmark's wall time despite
 having far fewer batch calls than vitals (see 'docs/benchmark-notes.md's
 new stage for the measured per-source breakdown).

 Measured directly (a same-session run at @TIERED_BENCH_SCALE=0.1@, 100
 patients, bundling on, width 1, multiplier 1.0): 79,715 total batch calls,
 44.438 s cold eval -- __0.558 ms\/batch call__ averaged across the five
 tiers, far below @threadDelay@\'s ~1.28 ms floor, as expected once real
 tiers are in effect. Solving @totalBatchCalls(patients) x 0.000558 s ~= 80
 s@ against the measured ~797 batch calls\/patient (@79715 \/ 100@ -- almost
 exactly the same per-patient figure the 'threadDelay'-era estimate found,
 since batch-call count is a structural property of the graph, independent
 of which latency mechanism serves it) gives ~180 patients;
 @'defaultScale' = 0.18@ (180 patients, 4 wards) was chosen as that exact
 value -- no further rounding needed -- and confirmed directly (143,477
 batch calls, 80.4 s cold eval) to land squarely inside the one-to-two-minute
 window at the default multiplier (see 'docs/benchmark-notes.md's new
 stage). @TIERED_BENCH_SCALE=1.0@ (Hospital's own default patient count)
 remains available and is cheap at @TIERED_BENCH_LATENCY_MULT=0@ for a
 structural-only run -- 'simulateRoundTrip'\'s @latencyUs > 0@ guard means no
 delay call, safe-FFI or otherwise, is ever made at multiplier @0@, so
 neither the tiers nor their @usleep@ overhead are paid in that mode.
-}
module Control.Computations.Demos.Bench.Tiered (tieredBenchMain) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.Demos.Bench.SystemSrc
import Control.Computations.FlowImpls.HashMapFlow hiding (Key, Val)
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Bits (unsafeShiftL, unsafeShiftR, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time.Clock
import Data.Word (Word32, Word64)
import GHC.Stats
import System.Environment (lookupEnv)
import System.Posix.Process (getProcessID)
import System.Process (readProcess)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

----------------------------------------------------------------------------
-- Packing (identical to Hospital.hs's own -- see that module's haddock for
-- the full "why a bare Word64, not a tuple or newtype" reasoning).
----------------------------------------------------------------------------

packWord32Pair :: Word32 -> Word32 -> Word64
packWord32Pair hi lo = (fromIntegral hi `unsafeShiftL` 32) .|. fromIntegral lo
{-# INLINE packWord32Pair #-}

unpackWord32Pair :: Word64 -> (Word32, Word32)
unpackWord32Pair w = (fromIntegral (w `unsafeShiftR` 32), fromIntegral w)
{-# INLINE unpackWord32Pair #-}

type PatSubKey = Word64

mkPatSubKey :: PatId -> Word32 -> PatSubKey
mkPatSubKey = packWord32Pair
{-# INLINE mkPatSubKey #-}

patOf :: PatSubKey -> PatId
patOf = fst . unpackWord32Pair
{-# INLINE patOf #-}

subOf :: PatSubKey -> Word32
subOf = snd . unpackWord32Pair
{-# INLINE subOf #-}

----------------------------------------------------------------------------
-- Graph shape -- identical constants to Hospital.hs, so the achieved
-- instance count and depth distribution are directly comparable at matching
-- scales (see the module haddock's "Shape preserved from Hospital" note).
----------------------------------------------------------------------------

type PatId = Word32
type WardId = Word32
type VitalId = Word32
type WindowId = Word32
type LabId = Word32
type MedId = Word32
type InteractionId = Word32
type NoteId = Word32

basePatientCount, baseWardCount :: Int
basePatientCount = 1000
baseWardCount = 20

vitalsPerPatient, windowsPerPatient, vitalsPerWindow :: Int
vitalsPerPatient = 250
windowsPerPatient = 50
vitalsPerWindow = vitalsPerPatient `div` windowsPerPatient

labsPerPatient :: Int
labsPerPatient = 180

medsPerPatient :: Int
medsPerPatient = 18

notesPerPatient :: Int
notesPerPatient = 140

allMedPairs :: [(MedId, MedId)]
allMedPairs = [(a, b) | a <- [0 .. medsPerPatient' - 1], b <- [a + 1 .. medsPerPatient' - 1]]
 where
  medsPerPatient' = fromIntegral medsPerPatient :: MedId

interactionsPerPatient :: Int
interactionsPerPatient = length allMedPairs

wardSizes :: Int -> Int -> [Int]
wardSizes wardCount patientCount = [base + (if w < extra then 1 else 0) | w <- [0 .. wardCount - 1]]
 where
  (base, extra) = patientCount `divMod` wardCount

wardOffsets :: Int -> Int -> [Int]
wardOffsets wardCount patientCount = scanl (+) 0 (wardSizes wardCount patientCount)

patientsOfWard :: [Int] -> WardId -> [PatId]
patientsOfWard offsets w = map fromIntegral [lo .. hi - 1]
 where
  wi = fromIntegral w
  lo = offsets !! wi
  hi = offsets !! (wi + 1)

wardOfWith :: [Int] -> PatId -> WardId
wardOfWith offsets p = fromIntegral (length (takeWhile (<= fromIntegral p) (drop 1 offsets)))

labTrendChainCap :: PatId -> Int
labTrendChainCap p = 1 + fromIntegral (p `mod` 5)

----------------------------------------------------------------------------
-- Source/sink ids and keys
----------------------------------------------------------------------------

adtSrcId, vitalsSrcId, labsSrcId, pharmacySrcId, notesSrcId :: TypedCompSrcId SystemSrc
adtSrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "tiered-bench-adt"
vitalsSrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "tiered-bench-vitals"
labsSrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "tiered-bench-labs"
pharmacySrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "tiered-bench-pharmacy"
notesSrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "tiered-bench-notes"

outSinkId :: TypedCompSinkId HashMapFlow
outSinkId = unsafeMkTypedCompSinkId (Proxy @HashMapFlow) "tiered-bench-out"

patKey :: PatId -> BS.ByteString
patKey p = BSC.pack ("p" ++ show p)

adtKey :: PatId -> Key
adtKey p = BSC.pack ("adt/p" ++ show p)

vitalValueKey, vitalUnitKey, vitalRangeKey :: (PatId, VitalId) -> Key
vitalValueKey (p, v) = BSC.pack ("vitals/value/p" ++ show p ++ "/v" ++ show v)
vitalUnitKey (p, v) = BSC.pack ("vitals/unit/p" ++ show p ++ "/v" ++ show v)
vitalRangeKey (p, v) = BSC.pack ("vitals/range/p" ++ show p ++ "/v" ++ show v)

-- | The new read 'vitalWindowComp' gains -- see the module haddock's "Mixed
-- dependencies" table. One key per patient (not per window), read by every
-- one of that patient's 50 'vitalWindowComp' instances.
calibrationKey :: PatId -> Key
calibrationKey p = BSC.pack ("vitals/calibration/p" ++ show p)

labResultKey, labRangeKey, labSpecimenKey :: (PatId, LabId) -> Key
labResultKey (p, l) = BSC.pack ("labs/result/p" ++ show p ++ "/l" ++ show l)
labRangeKey (p, l) = BSC.pack ("labs/range/p" ++ show p ++ "/l" ++ show l)
labSpecimenKey (p, l) = BSC.pack ("labs/specimen/p" ++ show p ++ "/l" ++ show l)

-- | The one key every 'riskScoreComp' instance in the whole run reads --
-- see the module haddock's NOTE on why this is deliberate, not an oversight.
riskModelKey :: Key
riskModelKey = BSC.pack "labs/risk-model-coefficients"

medOrderKey, medDrugKey :: (PatId, MedId) -> Key
medOrderKey (p, m) = BSC.pack ("pharmacy/order/p" ++ show p ++ "/m" ++ show m)
medDrugKey (p, m) = BSC.pack ("pharmacy/drug/p" ++ show p ++ "/m" ++ show m)

-- | The new read 'interactionComp' gains -- see the module haddock's "Mixed
-- dependencies" table and "Scale" section (this is the read that dominates
-- this benchmark's wall time).
interactionRowKey :: (PatId, InteractionId) -> Key
interactionRowKey (p, i) = BSC.pack ("pharmacy/interaction/p" ++ show p ++ "/i" ++ show i)

noteTextKey, noteAuthorKey :: (PatId, NoteId) -> Key
noteTextKey (p, n) = BSC.pack ("notes/text/p" ++ show p ++ "/n" ++ show n)
noteAuthorKey (p, n) = BSC.pack ("notes/author/p" ++ show p ++ "/n" ++ show n)

-- | The new read 'wardCensusComp' gains -- see the module haddock's "Mixed
-- dependencies" table. One key per ward.
bedCapacityKey :: WardId -> Key
bedCapacityKey w = BSC.pack ("adt/bed-capacity/w" ++ show w)

-- | See Hospital.hs's own 'valOf' for the full rationale (no source is
-- pre-populated; every read resolves via this deterministic fallback).
valOf :: Key -> Maybe Val -> Val
valOf key = fromMaybe (BS.take 8 key)

valWord64 :: Val -> Word64
valWord64 = BS.foldl' (\acc w -> acc * 31 + fromIntegral w) 0

----------------------------------------------------------------------------
-- Comp defs -- identical to Hospital.hs except the four marked NEW READ,
-- which each fold one extra source leaf into an existing applicative batch
-- (see the module haddock's "Mixed dependencies" table).
----------------------------------------------------------------------------

admissionCompDef :: [Int] -> CompDef PatId Word64
admissionCompDef offsets = defineComp "admissionComp" fullCaching $ \p -> do
  mval <- compSrcReq adtSrcId (SystemLookupReq (adtKey p))
  let v = valOf (adtKey p) mval
  pure (packWord32Pair (wardOfWith offsets p) (fromIntegral (BS.length v)))

vitalCompDef :: CompDef PatSubKey Word64
vitalCompDef = defineComp "vitalComp" fullCaching $ \pv -> do
  let p = patOf pv; v = subOf pv
  value <- compSrcReq vitalsSrcId (SystemLookupReq (vitalValueKey (p, v)))
  unit <- compSrcReq vitalsSrcId (SystemLookupReq (vitalUnitKey (p, v)))
  range <- compSrcReq vitalsSrcId (SystemLookupReq (vitalRangeKey (p, v)))
  pure
    ( valWord64 (valOf (vitalValueKey (p, v)) value)
        + valWord64 (valOf (vitalUnitKey (p, v)) unit)
        + valWord64 (valOf (vitalRangeKey (p, v)) range)
    )

-- | __NEW READ__: the patient's monitor calibration (vitals), alongside the
-- existing 5-way fan-in over 'vitalComp' -- see the module haddock.
vitalWindowCompDef :: Comp PatSubKey Word64 -> CompDef PatSubKey Word64
vitalWindowCompDef vitalC = defineComp "vitalWindowComp" fullCaching $ \pw -> do
  let p = patOf pw; w = subOf pw :: WindowId
      vitalsPerWindow' = fromIntegral vitalsPerWindow :: VitalId
      vitalIds = [w * vitalsPerWindow' .. w * vitalsPerWindow' + vitalsPerWindow' - 1]
  calibration <- compSrcReq vitalsSrcId (SystemLookupReq (calibrationKey p))
  readings <- traverse (\v -> evalCompOrFail vitalC (mkPatSubKey p v)) vitalIds
  pure (sum readings + valWord64 (valOf (calibrationKey p) calibration))

labResultCompDef :: CompDef PatSubKey Word64
labResultCompDef = defineComp "labResultComp" fullCaching $ \pl -> do
  let p = patOf pl; l = subOf pl
  result <- compSrcReq labsSrcId (SystemLookupReq (labResultKey (p, l)))
  range <- compSrcReq labsSrcId (SystemLookupReq (labRangeKey (p, l)))
  specimen <- compSrcReq labsSrcId (SystemLookupReq (labSpecimenKey (p, l)))
  pure
    ( valWord64 (valOf (labResultKey (p, l)) result)
        + valWord64 (valOf (labRangeKey (p, l)) range)
        + valWord64 (valOf (labSpecimenKey (p, l)) specimen)
    )

labTrendCompDef
  :: Comp PatSubKey Word64 -> Comp PatSubKey Word64 -> CompDef PatSubKey Word64
labTrendCompDef labResultC labTrendC = defineComp "labTrendComp" fullCaching $ \pl -> do
  let p = patOf pl; l = subOf pl
      cap = labTrendChainCap p
      s = fromIntegral l `mod` cap
  resultVal <- evalCompOrFail labResultC pl
  if s == 0
    then pure resultVal
    else do
      prev <- evalCompOrFail labTrendC (mkPatSubKey p (l - 1))
      pure (prev + resultVal)

medOrderCompDef :: CompDef PatSubKey Word64
medOrderCompDef = defineComp "medOrderComp" fullCaching $ \pm -> do
  let p = patOf pm; m = subOf pm
  order <- compSrcReq pharmacySrcId (SystemLookupReq (medOrderKey (p, m)))
  drug <- compSrcReq pharmacySrcId (SystemLookupReq (medDrugKey (p, m)))
  pure (valWord64 (valOf (medOrderKey (p, m)) order) + valWord64 (valOf (medDrugKey (p, m)) drug))

-- | __NEW READ__: the drug-pair interaction row (pharmacy), alongside the
-- existing two 'medOrderComp' reads -- see the module haddock. This is the
-- comp instance the "Scale" section's arithmetic is dominated by: 153
-- instances/patient, each its own nested cap evaluation, so none of these
-- pharmacy reads can bundle with each other or overlap via concurrency
-- width (see "Control.Computations.CompEngine.Impl"'s @doSuspended@ -- only
-- source leaves *within the same* applicative batch can ever be dispatched
-- concurrently, and each of these 153 has exactly one).
interactionCompDef :: Comp PatSubKey Word64 -> CompDef PatSubKey Bool
interactionCompDef medOrderC = defineComp "interactionComp" fullCaching $ \pi_ -> do
  let p = patOf pi_; i = subOf pi_ :: InteractionId
      (m1, m2) = allMedPairs !! fromIntegral i
  row <- compSrcReq pharmacySrcId (SystemLookupReq (interactionRowKey (p, i)))
  r1 <- evalCompOrFail medOrderC (mkPatSubKey p m1)
  r2 <- evalCompOrFail medOrderC (mkPatSubKey p m2)
  let rowVal = valWord64 (valOf (interactionRowKey (p, i)) row)
  pure ((r1 == r2) /= odd rowVal)

noteCompDef :: CompDef PatSubKey Word64
noteCompDef = defineComp "noteComp" fullCaching $ \pn -> do
  let p = patOf pn; n = subOf pn
  text <- compSrcReq notesSrcId (SystemLookupReq (noteTextKey (p, n)))
  author <- compSrcReq notesSrcId (SystemLookupReq (noteAuthorKey (p, n)))
  pure (valWord64 (valOf (noteTextKey (p, n)) text) + valWord64 (valOf (noteAuthorKey (p, n)) author))

noteDigestCompDef :: Comp PatSubKey Word64 -> CompDef PatId Word64
noteDigestCompDef noteC = defineComp "noteDigestComp" fullCaching $ \p -> do
  notes <- traverse (\n -> evalCompOrFail noteC (mkPatSubKey p n)) [0 .. fromIntegral notesPerPatient - 1]
  pure (sum notes)

-- | __NEW READ__: the shared risk-model coefficients (labs, one key for the
-- whole run) alongside the existing three fan-ins -- see the module
-- haddock's NOTE on why 'riskModelKey' is deliberately shared.
riskScoreCompDef
  :: Comp PatSubKey Word64
  -> Comp PatSubKey Word64
  -> Comp PatSubKey Bool
  -> CompDef PatId Word64
riskScoreCompDef vitalWindowC labTrendC interactionC = defineComp "riskScoreComp" fullCaching $ \p -> do
  coeffs <- compSrcReq labsSrcId (SystemLookupReq riskModelKey)
  windows <- traverse (\w -> evalCompOrFail vitalWindowC (mkPatSubKey p w)) [0 .. fromIntegral windowsPerPatient - 1]
  trends <- traverse (\l -> evalCompOrFail labTrendC (mkPatSubKey p l)) [0 .. fromIntegral labsPerPatient - 1]
  interactions <- traverse (\i -> evalCompOrFail interactionC (mkPatSubKey p i)) [0 .. fromIntegral interactionsPerPatient - 1]
  pure
    ( sum windows
        + sum trends
        + fromIntegral (length (filter id interactions))
        + valWord64 (valOf riskModelKey coeffs)
    )

patientSummaryCompDef
  :: Comp PatId Word64 -> Comp PatId Word64 -> Comp PatId Word64 -> CompDef PatId Word64
patientSummaryCompDef riskScoreC admissionC noteDigestC =
  defineComp "patientSummaryComp" fullCaching $ \p -> do
    risk <- evalCompOrFail riskScoreC p
    admission <- evalCompOrFail admissionC p
    noteDigest <- evalCompOrFail noteDigestC p
    adtVal <- compSrcReq adtSrcId (SystemLookupReq (adtKey p))
    vitalsVal <- compSrcReq vitalsSrcId (SystemLookupReq (vitalValueKey (p, 0)))
    labsVal <- compSrcReq labsSrcId (SystemLookupReq (labResultKey (p, 0)))
    pharmacyVal <- compSrcReq pharmacySrcId (SystemLookupReq (medOrderKey (p, 0)))
    notesVal <- compSrcReq notesSrcId (SystemLookupReq (noteTextKey (p, 0)))
    let (ward, _admitLen) = unpackWord32Pair admission
        crossLen =
          valWord64 (valOf (adtKey p) adtVal)
            + valWord64 (valOf (vitalValueKey (p, 0)) vitalsVal)
            + valWord64 (valOf (labResultKey (p, 0)) labsVal)
            + valWord64 (valOf (medOrderKey (p, 0)) pharmacyVal)
            + valWord64 (valOf (noteTextKey (p, 0)) notesVal)
        summary = risk + fromIntegral ward + noteDigest + crossLen
    void $ compSinkReq outSinkId (HashMapStoreReq (patKey p) (BSC.pack (show summary)))
    pure summary

patientAlertCompDef :: Comp PatId Word64 -> Comp PatId Word64 -> CompDef PatId Bool
patientAlertCompDef riskScoreC admissionC = defineComp "patientAlertComp" fullCaching $ \p -> do
  risk <- evalCompOrFail riskScoreC p
  admission <- evalCompOrFail admissionC p
  let (ward, _) = unpackWord32Pair admission
  pure (risk `mod` 7 == fromIntegral ward `mod` 7)

-- | __NEW READ__: the ward's bed capacity (adt) alongside the existing
-- fan-in over 'admissionComp' -- see the module haddock.
wardCensusCompDef :: Comp PatId Word64 -> [Int] -> CompDef WardId Word64
wardCensusCompDef admissionC offsets = defineComp "wardCensusComp" fullCaching $ \w -> do
  capacity <- compSrcReq adtSrcId (SystemLookupReq (bedCapacityKey w))
  admissions <- traverse (evalCompOrFail admissionC) (patientsOfWard offsets w)
  pure (fromIntegral (length admissions) + valWord64 (valOf (bedCapacityKey w) capacity) `mod` 100)

wardOccupancyCompDef :: Comp PatId Word64 -> [Int] -> CompDef WardId Word64
wardOccupancyCompDef admissionC offsets = defineComp "wardOccupancyComp" fullCaching $ \w -> do
  admissions <- traverse (evalCompOrFail admissionC) (patientsOfWard offsets w)
  pure (sum [fromIntegral (snd (unpackWord32Pair a)) | a <- admissions])

wardRiskBoardCompDef :: Comp PatId Bool -> [Int] -> CompDef WardId Word64
wardRiskBoardCompDef patientAlertC offsets = defineComp "wardRiskBoardComp" fullCaching $ \w -> do
  alerts <- traverse (evalCompOrFail patientAlertC) (patientsOfWard offsets w)
  pure (fromIntegral (length (filter id alerts)))

hospitalDashboardCompDef
  :: Comp WardId Word64 -> Comp WardId Word64 -> Comp WardId Word64 -> Int -> CompDef () Word64
hospitalDashboardCompDef wardCensusC wardRiskBoardC wardOccupancyC wardCount =
  defineComp "hospitalDashboardComp" fullCaching $ \() -> do
    censuses <- traverse (evalCompOrFail wardCensusC) [0 .. fromIntegral wardCount - 1]
    riskBoards <- traverse (evalCompOrFail wardRiskBoardC) [0 .. fromIntegral wardCount - 1]
    occupancies <- traverse (evalCompOrFail wardOccupancyC) [0 .. fromIntegral wardCount - 1]
    pure (sum censuses + sum riskBoards + sum occupancies)

transferCandidatesCompDef :: Comp PatId Word64 -> Comp PatId Word64 -> Int -> CompDef () Word64
transferCandidatesCompDef patientSummaryC admissionC patientCount =
  defineComp "transferCandidatesComp" fullCaching $ \() -> do
    summaries <- traverse (evalCompOrFail patientSummaryC) [0 .. fromIntegral patientCount - 1]
    admissions <- traverse (evalCompOrFail admissionC) [0 .. fromIntegral patientCount - 1]
    pure
      ( fromIntegral
          ( length
              [ ()
              | (s, a) <- zip summaries admissions
              , let (ward, _) = unpackWord32Pair a
              , s > fromIntegral ward
              ]
          )
      )

rootCompDef :: Comp () Word64 -> Comp () Word64 -> CompDef () ()
rootCompDef hospitalDashboardC transferCandidatesC = defineComp "root" fullCaching $ \() -> do
  _ <- evalCompOrFail hospitalDashboardC ()
  _ <- evalCompOrFail transferCandidatesC ()
  pure ()

wireTieredComps :: Int -> Int -> CompWireM (Comp () ())
wireTieredComps patientCount wardCount = do
  let offsets = wardOffsets wardCount patientCount
  admissionC <- wireComp (admissionCompDef offsets)
  vitalC <- wireComp vitalCompDef
  vitalWindowC <- wireComp (vitalWindowCompDef vitalC)
  labResultC <- wireComp labResultCompDef
  labTrendC <- defineRecursiveComp (labTrendCompDef labResultC)
  medOrderC <- wireComp medOrderCompDef
  interactionC <- wireComp (interactionCompDef medOrderC)
  noteC <- wireComp noteCompDef
  noteDigestC <- wireComp (noteDigestCompDef noteC)
  riskScoreC <- wireComp (riskScoreCompDef vitalWindowC labTrendC interactionC)
  patientSummaryC <- wireComp (patientSummaryCompDef riskScoreC admissionC noteDigestC)
  patientAlertC <- wireComp (patientAlertCompDef riskScoreC admissionC)
  wardCensusC <- wireComp (wardCensusCompDef admissionC offsets)
  wardOccupancyC <- wireComp (wardOccupancyCompDef admissionC offsets)
  wardRiskBoardC <- wireComp (wardRiskBoardCompDef patientAlertC offsets)
  hospitalDashboardC <-
    wireComp (hospitalDashboardCompDef wardCensusC wardRiskBoardC wardOccupancyC wardCount)
  transferCandidatesC <- wireComp (transferCandidatesCompDef patientSummaryC admissionC patientCount)
  wireComp (rootCompDef hospitalDashboardC transferCandidatesC)

----------------------------------------------------------------------------
-- Depth distribution -- byte-for-byte the same derivation as Hospital.hs's
-- own 'Control.Computations.Demos.Bench.Hospital.depthHistogram': the four
-- new source reads are all at level 0 (a source read is never itself a
-- dependency level), so folding them into an existing batch changes no
-- comp's own level -- see the module haddock's "Shape preserved from
-- Hospital" note. Duplicated rather than imported for the same reason as
-- the rest of this module's graph-shape code.
----------------------------------------------------------------------------

depthHistogram :: Int -> Int -> Map.Map Int Int
depthHistogram wardCount patientCount =
  Map.fromListWith
    (+)
    ( level1
        ++ level2Fixed
        ++ labTrendLevels
        ++ riskScoreLevels
        ++ summaryAlertLevels
        ++ topLevels
    )
 where
  caps = [labTrendChainCap (fromIntegral p) | p <- [0 .. patientCount - 1]]
  maxCap = maximum caps
  offsets = wardOffsets wardCount patientCount
  wardPatientIdxLists = [[offsets !! w .. offsets !! (w + 1) - 1] | w <- [0 .. wardCount - 1]]
  wardRiskBoardLevels =
    [1 + maximum [labTrendChainCap (fromIntegral p) + 3 | p <- ps] | ps <- wardPatientIdxLists]
  transferCandidatesLevel = maxCap + 4
  dashboardLevel = 1 + maximum (2 : wardRiskBoardLevels)
  rootLevel = 1 + max dashboardLevel transferCandidatesLevel
  level1 =
    [ (1, patientCount)
    , (1, patientCount * vitalsPerPatient)
    , (1, patientCount * labsPerPatient)
    , (1, patientCount * medsPerPatient)
    , (1, patientCount * notesPerPatient)
    ]
  level2Fixed =
    [ (2, patientCount * windowsPerPatient)
    , (2, patientCount * interactionsPerPatient)
    , (2, patientCount)
    , (2, wardCount)
    , (2, wardCount)
    ]
  labTrendLevels = [(2 + s, labsPerPatient `div` cap) | cap <- caps, s <- [0 .. cap - 1]]
  riskScoreLevels = [(cap + 2, 1) | cap <- caps]
  summaryAlertLevels = concat [[(cap + 3, 1), (cap + 3, 1)] | cap <- caps]
  topLevels =
    [(lvl, 1) | lvl <- wardRiskBoardLevels]
      ++ [(transferCandidatesLevel, 1), (dashboardLevel, 1), (rootLevel, 1)]

----------------------------------------------------------------------------
-- Flow wiring -- per-source latency tiers, jitter, and bundling, all
-- configured via 'SystemSrcConfig' (see the module haddock's "Per-source
-- latency tiers" and "The bundling knob" sections).
----------------------------------------------------------------------------

data TieredSrcs = TieredSrcs
  { tsrc_adt :: SystemSrc
  , tsrc_vitals :: SystemSrc
  , tsrc_labs :: SystemSrc
  , tsrc_pharmacy :: SystemSrc
  , tsrc_notes :: SystemSrc
  }

namedSrcs :: TieredSrcs -> [(String, SystemSrc)]
namedSrcs srcs =
  [ ("adt", tsrc_adt srcs)
  , ("vitals", tsrc_vitals srcs)
  , ("labs", tsrc_labs srcs)
  , ("pharmacy", tsrc_pharmacy srcs)
  , ("notes", tsrc_notes srcs)
  ]

-- | Base (multiplier @1.0@) per-source latency in microseconds -- see the
-- module haddock's "Per-source latency tiers" table. Roughly a 40x spread
-- (25 to 1000 microseconds), 1 ms chosen as the ceiling per this task's
-- brief.
baseLatencyUsFor :: String -> Int
baseLatencyUsFor name = case name of
  "vitals" -> 25
  "notes" -> 100
  "labs" -> 250
  "adt" -> 500
  "pharmacy" -> 1000
  other -> error ("baseLatencyUsFor: unknown source " ++ other)

initTieredSrcs :: Double -> Bool -> Bool -> IO TieredSrcs
initTieredSrcs latencyMult jitterEnabled batchingEnabled =
  TieredSrcs
    <$> initFor adtSrcId "adt"
    <*> initFor vitalsSrcId "vitals"
    <*> initFor labsSrcId "labs"
    <*> initFor pharmacySrcId "pharmacy"
    <*> initFor notesSrcId "notes"
 where
  initFor sid name =
    initSystemSrcWith
      (instTextFromTypedCompSrcId sid)
      SystemSrcConfig
        { ssc_latencyUs = round (fromIntegral (baseLatencyUsFor name) * latencyMult :: Double)
        , ssc_jitterEnabled = jitterEnabled
        , ssc_batchingEnabled = batchingEnabled
        }

totalSystemCallCount :: TieredSrcs -> IO Int
totalSystemCallCount srcs = sum <$> traverse (sysCallCount . snd) (namedSrcs srcs)

totalSystemBatchCallCount :: TieredSrcs -> IO Int
totalSystemBatchCallCount srcs = sum <$> traverse (sysBatchCallCount . snd) (namedSrcs srcs)

----------------------------------------------------------------------------
-- Rerun-heavy live phase (phase 3) -- mirrors Hospital.hs's own phase 3
-- exactly (same mutation-spreading scheme, same settle discipline); see
-- that module's own extensive comment for the full reasoning, not repeated
-- here.
----------------------------------------------------------------------------

defaultRerunKeys :: Int
defaultRerunKeys = 400

rerunMutationVal :: Int -> Val
rerunMutationVal n = BSC.pack ("rerun-" ++ show n ++ "-") <> BSC.replicate 32 'r'

rerunMutationTarget :: TieredSrcs -> Int -> Int -> (SystemSrc, Key)
rerunMutationTarget srcs patientCount n =
  case n `mod` 5 of
    0 -> (tsrc_adt srcs, adtKey p)
    1 -> (tsrc_vitals srcs, vitalValueKey (p, subId `mod` fromIntegral vitalsPerPatient))
    2 -> (tsrc_labs srcs, labResultKey (p, subId `mod` fromIntegral labsPerPatient))
    3 -> (tsrc_pharmacy srcs, medOrderKey (p, subId `mod` fromIntegral medsPerPatient))
    _ -> (tsrc_notes srcs, noteTextKey (p, subId `mod` fromIntegral notesPerPatient))
 where
  p = fromIntegral ((n * 9973) `mod` patientCount) :: PatId
  subId = fromIntegral (n `div` 5) :: Word32

withTieredFlows :: TieredSrcs -> HashMapFlow -> Int -> CompFlowRegistry -> IO () -> IO ()
withTieredFlows srcs sink width reg action = do
  setCompFlowConcurrency reg (mkCompFlowConcurrency width)
  forM_ (namedSrcs srcs) (registerCompSrc reg . snd)
  registerCompSink reg sink
  action

----------------------------------------------------------------------------
-- Counting driver -- duplicated from Hospital.hs's own copy (itself
-- duplicated from Bench.Main's, for the same reason: none of the three is
-- exported, and none of the three may change to accommodate another).
----------------------------------------------------------------------------

countingStateIf :: IORef Int -> CompEngineStateIf IO -> CompEngineStateIf IO
countingStateIf ref orig =
  CompEngineStateIf
    { lookupCapResult = lookupCapResult orig
    , capEvaluationStarted = \cap -> do
        atomicModifyIORef' ref (\n -> (n + 1, ()))
        capEvaluationStarted orig cap
    , capEvaluationFinished = capEvaluationFinished orig
    , dequeueGivenCap = dequeueGivenCap orig
    , dequeueNextCap = dequeueNextCap orig
    , staleQueueSize = staleQueueSize orig
    , enqueueStaleCaps = enqueueStaleCaps orig
    , trackOutput = trackOutput orig
    , getCompSinkOuts = getCompSinkOuts orig
    , getQueue = getQueue orig
    }

tieredBenchDriver
  :: IORef Int
  -> TVar (Option RunStats)
  -> (CompFlowRegistry -> IO () -> IO ())
  -> CompWireM (Comp () ())
  -> IO ()
tieredBenchDriver counterRef runVar withRegisteredFlows wireComps = do
  reg <- newCompFlowRegistry
  withStateIf $ \stateIf -> withRegisteredFlows reg $ do
    let ifs =
          CompEngineIfs
            { ce_compFlowRegistry = reg
            , ce_stateIf = countingStateIf counterRef stateIf
            }
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = shouldStartNextRun stateIf reg runVar
            , rcif_emptyChangesMode = Block
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = seconds 10
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = garbageHandler reg
            }
    comps <- rootComps
    runCompEngine ifs comps rifs ()
 where
  runDeletes stateIf reg = forAllSinks_ reg (deleteDeadOutputs stateIf)
  shouldStartNextRun stateIf reg runVarLocal nRun hadChanges nStaleCaps state = do
    when (nRun == 1) (runDeletes stateIf reg)
    let !stats = RunStats{rs_run = nRun, rs_hadChanges = hadChanges, rs_staleCaps = nStaleCaps}
    atomically $ writeTVar runVarLocal (Some stats)
    pure (startNextRun, state)
  rootComps = (failInM . fmap snd) $
    runCompWireM $
      do
        c <- wireComps
        pure [wrapCompAp (mkCompAp c ())]

----------------------------------------------------------------------------
-- Environment
----------------------------------------------------------------------------

readEnvDouble :: String -> Double -> IO Double
readEnvDouble name def = maybe def id . (>>= readMaybe) <$> lookupEnv name

readEnvIntAtLeast :: Int -> String -> Int -> IO Int
readEnvIntAtLeast lowerBound name def =
  maybe def (max lowerBound) . (>>= readMaybe) <$> lookupEnv name

readEnvBool :: String -> Bool -> IO Bool
readEnvBool name def = maybe def (`notElem` ["", "0"]) <$> lookupEnv name

-- | See the module haddock's "Scale" section for the arithmetic behind
-- @0.18@ -- measured directly against the safe-FFI @usleep@ latencies
-- 'Control.Computations.Demos.Bench.SystemSrc' actually pays (not
-- 'Control.Concurrent.threadDelay's ~1.28 ms floor, which no longer applies
-- now that this module's sources sleep via @usleep@).
defaultScale :: Double
defaultScale = 0.18

scaledPatientCount :: Double -> Int
scaledPatientCount scale = max 1 (round (fromIntegral basePatientCount * scale :: Double))

scaledWardCount :: Double -> Int -> Int
scaledWardCount scale patientCount =
  min patientCount (max 1 (round (fromIntegral baseWardCount * scale :: Double)))

----------------------------------------------------------------------------
-- Memory measurement (duplicated -- see the counting driver's own note)
----------------------------------------------------------------------------

getRssMb :: IO Double
getRssMb = do
  pid <- getProcessID
  out <- readProcess "ps" ["-o", "rss=", "-p", show pid] ""
  pure $ case words out of
    (kbStr : _) -> maybe 0 (\kb -> fromIntegral (kb :: Int) / 1024) (readMaybe kbStr)
    [] -> 0

----------------------------------------------------------------------------
-- Orchestration
----------------------------------------------------------------------------

tieredBenchMain :: IO ()
tieredBenchMain = do
  scale <- readEnvDouble "TIERED_BENCH_SCALE" defaultScale
  latencyMult <- readEnvDouble "TIERED_BENCH_LATENCY_MULT" 1.0
  jitterEnabled <- readEnvBool "TIERED_BENCH_JITTER" False
  batchingEnabled <- readEnvBool "TIERED_BENCH_BUNDLING" True
  width <- readEnvIntAtLeast 1 "TIERED_BENCH_CONCURRENCY" 1
  let patientCount = scaledPatientCount scale
      wardCount = scaledWardCount scale patientCount
      histogram = depthHistogram wardCount patientCount
      targetInstances = sum (Map.elems histogram)

  putStrLn "=== bench: tiered pipeline (mixed leaves, heterogeneous-latency benchmark) ==="
  printf
    "TIERED_BENCH_SCALE=%.4f TIERED_BENCH_LATENCY_MULT=%.4f TIERED_BENCH_JITTER=%s TIERED_BENCH_BUNDLING=%s TIERED_BENCH_CONCURRENCY=%d\n"
    scale
    latencyMult
    (show jitterEnabled)
    (show batchingEnabled)
    width
  printf
    "patients: %d, wards: %d (avg %.1f patients/ward), target instances (analytic): %d\n"
    patientCount
    wardCount
    (fromIntegral patientCount / fromIntegral wardCount :: Double)
    targetInstances
  putStrLn "per-source latency (base x multiplier, microseconds):"
  forM_ ["adt", "vitals", "labs", "pharmacy", "notes"] $ \name ->
    printf
      "  %-10s base=%-6d effective=%d\n"
      name
      (baseLatencyUsFor name)
      (round (fromIntegral (baseLatencyUsFor name) * latencyMult :: Double) :: Int)
  putStrLn "depth distribution (level -> instance count):"
  forM_ (Map.toAscList histogram) $ \(lvl, n) -> printf "  level %2d: %d\n" lvl n
  putStrLn ""

  srcs <- initTieredSrcs latencyMult jitterEnabled batchingEnabled
  sink <- initHashMapFlow (instTextFromTypedCompSinkId outSinkId)

  runVar <- newTVarIO None
  counterRef <- newIORef 0
  t0 <- getCurrentTime
  allocated0 <- allocated_bytes <$> getRTSStats
  engineHandle <-
    async
      ( tieredBenchDriver
          counterRef
          runVar
          (withTieredFlows srcs sink width)
          (wireTieredComps patientCount wardCount)
      )

  -- Cold settle: same reasoning as Hospital.hs's own phase 1.
  rs1 <- waitForRunAtLeast runVar 1
  tCold <- getCurrentTime
  coldReruns <- readIORef counterRef
  rssCold <- getRssMb
  allocatedCold <- allocated_bytes <$> getRTSStats
  let coldWallTime = realToFrac (diffUTCTime tCold t0) :: Double
      coldAllocated = allocatedCold - allocated0

  putStrLn "--- 1. cold eval ---"
  printf
    "achieved instance count: %d (target %d, %+.2f%%)\n"
    coldReruns
    targetInstances
    (100 * (fromIntegral coldReruns - fromIntegral targetInstances) / fromIntegral targetInstances :: Double)
  printf "wall time: %.3f s\n" coldWallTime
  printf "RSS after cold settle: %.1f MB\n" rssCold
  printf
    "allocated_bytes (cold eval): %d (%.1f MB)\n"
    coldAllocated
    (fromIntegral coldAllocated / 1000000 :: Double)

  docsAfterCold <- getHashMap sink
  let expectedDocs = patientCount
  when (length (hashMapKeys docsAfterCold) /= expectedDocs) $
    ioError
      ( userError
          ( "expected "
              ++ show expectedDocs
              ++ " patient summaries written to the sink, got "
              ++ show (length (hashMapKeys docsAfterCold))
          )
      )

  -- Settle-race workaround: see Hospital.hs's own ~40-line comment on its
  -- equivalent step (not repeated here, only relied upon).
  mRs2Advanced <- timeout 2000000 (waitForFullSettle runVar (rs_run rs1 + 1))
  rs2 <- case mRs2Advanced of
    Just rs -> pure rs
    Nothing -> waitForFullSettle runVar (rs_run rs1)
  preLiveReruns <- readIORef counterRef
  when (preLiveReruns /= coldReruns) $
    putStrLn
      ( "NOTE: settling before the live update triggered "
          ++ show (preLiveReruns - coldReruns)
          ++ " unexpected reruns (expected 0 -- no pre-population backlog exists here)"
      )

  -- 2. Live incremental update: mutate one vitals key.
  tBeforeMutate <- getCurrentTime
  allocatedPreLive <- allocated_bytes <$> getRTSStats
  sysInsert (tsrc_vitals srcs) (vitalValueKey (0, 0)) (BSC.replicate 40 'x')
  rs3 <- waitForFullSettle runVar (rs_run rs2 + 1)
  tAfterMutate <- getCurrentTime
  allocatedPostLive <- allocated_bytes <$> getRTSStats
  liveReruns <- readIORef counterRef
  let liveWallTime = realToFrac (diffUTCTime tAfterMutate tBeforeMutate) :: Double
      liveRerunCount = liveReruns - preLiveReruns
      liveAllocated = allocatedPostLive - allocatedPreLive

  putStrLn ""
  putStrLn "--- 2. live incremental, 1 changed vitals key ---"
  printf "wall time: %.4f s\n" liveWallTime
  printf "reruns: %d\n" liveRerunCount
  printf "allocated_bytes (live update): %d\n" liveAllocated

  rssFinal <- getRssMb
  rtsStats <- getRTSStats

  putStrLn ""
  putStrLn "--- memory ---"
  printf "RSS at end: %.1f MB\n" rssFinal
  printf
    "GHC max_live_bytes: %d (%.1f MB)\n"
    (max_live_bytes rtsStats)
    (fromIntegral (max_live_bytes rtsStats) / 1000000 :: Double)
  printf
    "GHC max_mem_in_use_bytes: %d (%.1f MB)\n"
    (max_mem_in_use_bytes rtsStats)
    (fromIntegral (max_mem_in_use_bytes rtsStats) / 1000000 :: Double)
  printf "GHC gcs: %d\n" (gcs rtsStats)
  when (coldReruns > 0) $
    printf
      "bytes/instance (GHC max_live_bytes / instances): %.1f B/instance\n"
      (fromIntegral (max_live_bytes rtsStats) / fromIntegral coldReruns :: Double)

  putStrLn ""
  putStrLn "--- source calls ---"
  totalCalls <- totalSystemCallCount srcs
  totalBatchCalls <- totalSystemBatchCallCount srcs
  printf "total source requests: %d\n" totalCalls
  printf "total batch calls: %d\n" totalBatchCalls
  when (totalBatchCalls > 0) $
    printf
      "requests / batch calls: %.2fx\n"
      (fromIntegral totalCalls / fromIntegral totalBatchCalls :: Double)
  forM_ (namedSrcs srcs) $ \(name, s) -> do
    calls <- sysCallCount s
    batchCalls <- sysBatchCallCount s
    hw <- sysHighWaterMark s
    printf
      "  %-10s requests=%-8d batch calls=%-8d concurrency high-water mark=%d\n"
      name
      calls
      batchCalls
      hw

  -- 3. Rerun-heavy live update -- mirrors Hospital.hs's own phase 3 exactly
  -- (see that module's comment for the full reasoning).
  rerunKeysPerRound <- readEnvIntAtLeast 0 "TIERED_BENCH_RERUN_KEYS" defaultRerunKeys
  rerunRounds <- readEnvIntAtLeast 1 "TIERED_BENCH_RERUN_LOOPS" 1
  when (rerunKeysPerRound > 0) $ do
    nextRunRef <- newIORef (rs_run rs3 + 1)
    roundRerunsRef <- newIORef (0 :: Int)
    tRerunStart <- getCurrentTime
    allocatedRerunStart <- allocated_bytes <$> getRTSStats
    forM_ [0 .. rerunRounds - 1] $ \round_ -> do
      before <- readIORef counterRef
      let batch =
            [ (src, key, rerunMutationVal globalIdx)
            | i <- [0 .. rerunKeysPerRound - 1]
            , let globalIdx = round_ * rerunKeysPerRound + i
                  (src, key) = rerunMutationTarget srcs patientCount globalIdx
            ]
      sysInsertBatch batch
      nextRun <- readIORef nextRunRef
      rsN <- waitForFullSettle runVar nextRun
      writeIORef nextRunRef (rs_run rsN + 1)
      after <- readIORef counterRef
      modifyIORef' roundRerunsRef (+ (after - before))
    tRerunEnd <- getCurrentTime
    allocatedRerunEnd <- allocated_bytes <$> getRTSStats
    rerunReruns <- readIORef roundRerunsRef
    let rerunWall = realToFrac (diffUTCTime tRerunEnd tRerunStart) :: Double
        totalKeysMutated = rerunKeysPerRound * rerunRounds
        rerunAllocated = allocatedRerunEnd - allocatedRerunStart
    putStrLn ""
    putStrLn "--- 3. rerun-heavy live update (multi-key, TIERED_BENCH_RERUN_KEYS/_LOOPS) ---"
    printf
      "keys mutated per round: %d, rounds: %d, total keys mutated: %d\n"
      rerunKeysPerRound
      rerunRounds
      totalKeysMutated
    printf "wall time: %.4f s\n" rerunWall
    printf "reruns: %d\n" rerunReruns
    printf
      "allocated_bytes: %d (%.1f MB)\n"
      rerunAllocated
      (fromIntegral rerunAllocated / 1000000 :: Double)
    when (rerunReruns > 0) $ do
      printf "us/rerun: %.3f\n" (rerunWall * 1e6 / fromIntegral rerunReruns :: Double)
      printf
        "allocated_bytes/rerun: %.1f B/rerun\n"
        (fromIntegral rerunAllocated / fromIntegral rerunReruns :: Double)

  cancel engineHandle
 where
  hashMapKeys = HashMap.keys
