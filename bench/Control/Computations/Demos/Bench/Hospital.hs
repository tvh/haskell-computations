{-# LANGUAGE ApplicativeDo #-}

{- | A second benchmark, alongside "Control.Computations.Demos.Bench.Main",
 that can actually show the effect of
 'Control.Computations.CompEngine.CompFlowRegistry.setCompFlowConcurrency':
 the existing benchmark builds zero applicative batches (its bodies are
 sequential monadic binds, going through 'compMBind' rather than the
 engine's own @<*>@\/@compMAp@) and its source has no latency to hide, so
 changing that benchmark's width knob is a documented no-op. This module's
 graph is built specifically so both of those stop being true.

 __Shape__: modelled on @app\/Control\/Computations\/Demos\/Hospital\/CompDefs.hs@
 (read that module for the flavour of a real client of this library) but
 with no expensive flows -- no sqlite, no filesystem. Five
 "Control.Computations.Demos.Bench.SystemSrc" instances stand in for
 separate clinical systems (admissions\/discharge\/transfer, vitals, labs,
 pharmacy, notes), each with a configurable per-call latency; a
 'HashMapFlow' is the sink. 1,000 patients across 20 wards, scaled by
 @HOSPITAL_BENCH_SCALE@ (see "Scale" below), target __~976 computation
 instances per patient__ (see 'depthHistogram'\'s haddock for the exact
 per-comp breakdown and why it sums to precisely 976) -- ~976,000 total at
 the default scale, the same order as the existing benchmark's ~1.14M.

 __Making the batches real__ is the part that decides whether this
 benchmark measures anything at all: this module enables
 @{-\# LANGUAGE ApplicativeDo \#-}@ and fans in dependencies with 'traverse',
 never @mapM_@\/@forM_@ (those discard results via @(>>)@, which for 'CompM'
 goes through the ordinary monadic 'compMBind' -- exactly why the existing
 benchmark's own @forM_@\/@mapM_@ never builds a
 'Control.Computations.CompEngine.Types.CompReqCombined' batch in the first
 place; see that module's own haddock). More importantly, most source reads
 here are genuinely multi-key: 'vitalComp' reads value\/unit\/reference-range
 (3 keys), 'labResultComp' reads result\/reference-range\/specimen (3 keys),
 'medOrderComp' reads order\/drug (2 keys), 'noteComp' reads text\/author (2
 keys) -- each such comp body's independent binds get desugared by
 @ApplicativeDo@ into one @\<*\>@-combined request against the /same/
 'SystemSrc' instance, which is exactly what 'FlowConcurrent' lets the
 engine dispatch as overlapping jobs at width > 1. 'patientSummaryComp' is
 the one deliberately /cross/-system batch: it reads one key from each of
 the five sources in a single applicative block. Without this design, only
 'patientSummaryComp' would ever batch across sources and the concurrent
 fraction of all source calls would be under 1% of the graph's ~1.6M source
 calls -- see 'hospitalBenchMain'\'s reported call counts for the actual
 achieved fraction.

 __Depth__: heterogeneous across ~11 levels (the existing benchmark is a
 uniform 10) -- see 'depthHistogram'.

 __Settle discipline__: this module reimplements a driver loop
 ('hospitalBenchDriver') and reruns the exact same @waitForRunAtLeast@\/
 @waitForFullSettle@ dance "Control.Computations.Demos.Bench.Main" uses
 before its own live-update measurement. That dance exists to route around a
 genuine race in how 'Control.Computations.CompEngine.Driver.compDriver''
 posts 'RunStats' -- see that module's ~40-line comment (around its own
 live-update section) for the full reproduction; it is not repeated here,
 only relied upon.

 __Rerun-heavy live phase__: phases 1-2 (cold eval, then one changed vitals
 key) leave the incremental engine's rerun path almost unexercised -- one
 mutated key cascades to only 8 reruns (vitalComp -> vitalWindowComp ->
 riskScoreComp -> patientSummaryComp\/patientAlertComp -> that patient's ward
 comps -> the dashboard), nowhere near enough to justify any rerun-path
 optimisation by measurement. A third phase (see 'hospitalBenchMain's own
 comment, and 'rerunMutationTarget'\/'defaultRerunKeys') mutates many keys in
 one batch -- spread across patients (and therefore wards) and across all
 five sources, via a stride chosen so a batch far smaller than the patient
 count still touches the full id range -- so the same fan-in that makes 1
 key worth 8 reruns makes a few hundred keys worth thousands. Configurable
 via @HOSPITAL_BENCH_RERUN_KEYS@ (keys per round, 0 disables the phase) and
 @HOSPITAL_BENCH_RERUN_LOOPS@ (repeated rounds, mirroring
 "Control.Computations.Demos.Bench.Main"'s @PERSIST_BENCH_LIVE_LOOPS@), and
 reported as keys mutated, wall time, reruns, and microseconds\/rerun -- the
 last being the number future rerun-path work should be judged against, and
 comparable round to round regardless of how @HOSPITAL_BENCH_RERUN_KEYS@ is
 tuned.

 = Memory: why the first cut of this benchmark was 13x heavier per instance

 The engine's per-def row storage
 ("Control.Computations.CompEngine.Utils.DefTable"'s @mkColumn@) unboxes a
 param\/value column only when the /whole/ type @e@ is one of a fixed
 whitelist -- @Word32@, @Word64@, @Int@, @Char@, @Bool@, @Double@ -- checked
 by 'Data.Typeable.eqT' against the literal type, not structurally. A
 @(Val, Val, Val)@ result or an @(Int, Int)@ param tuple fails that check
 regardless of how cheap its components are individually, so every such row
 falls back to a boxed 'Data.Vector.Mutable.IOVector': one heap-allocated
 cons per row for the tuple itself, plus one more per boxed field. On top of
 that a fat cached value costs more every time it's compared, hashed, or
 logged. This module's original params were @(Int, Int)@ and several
 results were @(Val, Val, Val)@ -- both land squarely outside the whitelist.

 The fix has two parts:

 * __Every genuinely multi-field comp param or result is packed into a
   single, bare 'Word64'__ (via 'packWord32Pair'\/'unpackWord32Pair'\/
   'mkPatSubKey'\/'patOf'\/'subOf' below) -- not a @(Word32, Word32)@ tuple,
   and not a @newtype@ wrapper around 'Word64' either. Both of those are
   *narrower* than @(Int, Int)@ but neither passes @mkColumn@'s literal-type
   check (a tuple is never in the whitelist regardless of its components'
   types, and 'Data.Typeable.eqT' does not see through a @newtype@), so
   *only* a bare 'Word64' actually reaches 'ColUnboxed'.
 * __@Int@\/@Bool@ single-field results were left alone.__ Both are already
   on @mkColumn@'s whitelist (an @Int@ column is exactly as unboxed as a
   @Word64@ column; a @Bool@ column is *cheaper* still -- one byte per row
   versus eight), so converting e.g. 'noteDigestComp'\'s @Int@ result to
   @Word64@ would cost lines of @fromIntegral@ noise for a memory delta of
   zero. 'interactionComp' and 'patientAlertComp' keep their 'Bool' results
   for the same reason. Numeric rollups that already consume a packed
   'Word64' upstream (e.g. 'riskScoreComp' summing 'vitalWindowComp'\'s
   results) are left as 'Word64' too, purely because that is the path with
   the least code, not because @Int@ would have cost more.

 Every two-key comp in this module (vital, vitalWindow, labResult\/labTrend,
 medOrder, interaction, note) reuses one packed key type, 'PatSubKey' --
 they are already distinct 'CompDef's (so nothing is lost by not giving each
 its own single-use @newtype@), and the existing benchmark makes the
 identical call for its own params\/results (@Comp Word32 Word64@
 throughout, one generic numeric type, no per-level newtypes).

 __Measured effect -- honest accounting, not a tuned number.__ At 48,806
 instances (scale 0.05): original @(Int, Int)@ params \/ @(Val, Val, Val)@
 results, @209.0 MB@ 'max_live_bytes'. Packing only the fat results to
 @Word64@ (params left as an @(Int, Int)@-shaped tuple): @199.4 MB@ -- almost
 the entire improvement. Packing params too (the change actually shipped):
 @198.3 MB@, another @~1.1 MB@. __Both parts together are a ~5% reduction,
 not the 13x this section's own mechanism would suggest, and nowhere near
 the existing benchmark's ~320 B\/instance.__ The gap is explained by
 something 'mkColumn' has nothing to do with: this benchmark deliberately
 never shares or pre-populates a source key (see 'valOf'\'s haddock), so
 essentially every one of its ~1.6M source reads interns a genuinely
 distinct, never-reused 'Key' -- a boxed 'ForAnyCompFlow' existential
 (source id + 'Data.Typeable.Proxy' + the 'ByteString' key) as a
 @Data.HashMap.Strict@ entry in
 "Control.Computations.CompEngine.Utils.SrcIndex"'s @KeyIntern@, *and* a
 second boxed copy in "DefTable.hs"'s own per-def @SrcDepIntern@ (see both
 modules' haddocks: each is a deliberate, correct design for the case those
 interned values are actually /shared/ across many rows -- exactly what the
 existing benchmark's 300-key @make_kv@ gives it, and exactly what this
 benchmark's realistic "every reading is its own clinical fact" design does
 not). Comparing @48,806@- and @97,609@-instance runs before this fix, the
 marginal cost tracked /source calls/, not instances: @+80,600@ source
 calls against @+48,803@ instances for @+208.5 MB@ is @~2,587 B@ per source
 call -- close enough to the observed ~4,270 B\/instance (at ~1.65 source
 calls\/instance averaged over this graph's mix of defs) to be the same
 number. That ratio barely moves after this fix (@~2,453 B@\/call), because
 packing params\/results never touched the src-dep interning tables at all.
 Fixing /that/ would mean changing how "DefTable.hs"\/"SrcIndex.hs" intern
 source keys -- explicitly out of scope here (@src\/@ is untouched, and the
 whole point of this benchmark's key design is /not/ to share keys -- see
 the module haddock's opening "Making the batches real" section). Reported
 here, not chased further, per this task's own instruction: an honest
 number beats a tuned one.

 = Scale

 @HOSPITAL_BENCH_SCALE@ scales /both/ the patient count and the ward count
 continuously (see 'scaledPatientCount'\/'scaledWardCount'), rather than
 fixing ward size at 50 and only scaling the ward count -- the latter
 bottoms out at one 50-patient ward as soon as @scale@ is small enough that
 @round (20 * scale)@ hits 0 (floored to 1), so e.g. @0.02@ and @0.05@ used
 to produce the *identical* 48,806-instance run despite being different
 knob values. A ward can now hold fewer than 50 patients at small scale (see
 'wardSizes'); the default @1.0@ still resolves to exactly 1,000 patients
 across 20 wards, 50 patients each, matching the original fixed scheme
 exactly.
-}
module Control.Computations.Demos.Bench.Hospital (hospitalBenchMain) where

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
-- Packing: the fix for Problem 1 (see the module haddock's "Memory"
-- section). One generic Word32-pair <-> Word64 primitive, reused both for
-- every two-key comp param (via PatSubKey/mkPatSubKey/patOf/subOf) and for
-- admissionComp's packed (ward, admission-length) result.
----------------------------------------------------------------------------

-- | Pack two 'Word32's into one 'Word64': @hi@ in the high 32 bits, @lo@ in
-- the low 32 bits. Deliberately a bare 'Word64', not a @newtype@ -- see the
-- module haddock for why that distinction is exactly what makes the
-- resulting column unboxed.
packWord32Pair :: Word32 -> Word32 -> Word64
packWord32Pair hi lo = (fromIntegral hi `unsafeShiftL` 32) .|. fromIntegral lo
{-# INLINE packWord32Pair #-}

-- | Inverse of 'packWord32Pair'.
unpackWord32Pair :: Word64 -> (Word32, Word32)
unpackWord32Pair w = (fromIntegral (w `unsafeShiftR` 32), fromIntegral w)
{-# INLINE unpackWord32Pair #-}

-- | A packed (patient id, per-patient sub-id) comp param -- see
-- 'packWord32Pair'. Reused across every two-key comp in this module
-- ('vitalComp', 'vitalWindowComp', 'labResultComp'\/'labTrendComp',
-- 'medOrderComp', 'interactionComp', 'noteComp'): all of them key on
-- @(PatId, some per-patient sub-id)@, and none needs to be told apart from
-- the others at the type level -- see the module haddock.
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
-- Graph shape
----------------------------------------------------------------------------

type PatId = Word32
type WardId = Word32
type VitalId = Word32
type WindowId = Word32
type LabId = Word32
type MedId = Word32
type InteractionId = Word32
type NoteId = Word32

-- | Default (scale 1.0) patient\/ward counts -- see 'scaledPatientCount'\/
-- 'scaledWardCount' and the module haddock's "Scale" section.
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

-- | All @18 choose 2@ = 153 distinct medication pairs, computed rather than
-- hardcoded so 'interactionsPerPatient' can never silently drift from
-- 'medsPerPatient'.
allMedPairs :: [(MedId, MedId)]
allMedPairs = [(a, b) | a <- [0 .. medsPerPatient' - 1], b <- [a + 1 .. medsPerPatient' - 1]]
 where
  medsPerPatient' = fromIntegral medsPerPatient :: MedId

interactionsPerPatient :: Int
interactionsPerPatient = length allMedPairs

-- | The number of patients in each of @wardCount@ wards, given @patientCount@
-- total: split as evenly as possible, the first @patientCount \`mod\`
-- wardCount@ wards getting one extra patient. At the default scale (1000
-- patients, 20 wards) this is a uniform 50 -- matching the original fixed
-- scheme exactly -- but at smaller scales a ward can hold fewer than 50 (see
-- the module haddock's "Scale" section). Callers are responsible for
-- ensuring @wardCount <= patientCount@ (see 'scaledWardCount') so every
-- ward gets at least 1 patient; 'wardOffsets' below would otherwise produce
-- an empty ward and 'depthHistogram'\'s per-ward @maximum@ would crash on
-- it.
wardSizes :: Int -> Int -> [Int]
wardSizes wardCount patientCount = [base + (if w < extra then 1 else 0) | w <- [0 .. wardCount - 1]]
 where
  (base, extra) = patientCount `divMod` wardCount

-- | Cumulative patient-id offset where each ward starts: length
-- @wardCount + 1@, first element 0, last element @patientCount@. Ward @w@
-- owns patient ids @[offsets !! w .. offsets !! (w + 1) - 1]@ -- see
-- 'patientsOfWard'\/'wardOfWith'.
wardOffsets :: Int -> Int -> [Int]
wardOffsets wardCount patientCount = scanl (+) 0 (wardSizes wardCount patientCount)

-- | Patient ids belonging to ward @w@, given its def's 'wardOffsets'.
patientsOfWard :: [Int] -> WardId -> [PatId]
patientsOfWard offsets w = map fromIntegral [lo .. hi - 1]
 where
  wi = fromIntegral w
  lo = offsets !! wi
  hi = offsets !! (wi + 1)

-- | Which ward patient @p@ belongs to, given 'wardOffsets'. @O(wardCount)@:
-- a linear scan over the (small, at most a few dozen even at large scale)
-- offsets list beats building an array for a lookup done at most
-- @patientCount@ times total (once per patient, inside 'admissionComp').
wardOfWith :: [Int] -> PatId -> WardId
wardOfWith offsets p = fromIntegral (length (takeWhile (<= fromIntegral p) (drop 1 offsets)))

{- | The lab-trend dependency chain's length for patient @p@, in @[1, 5]@ --
 this is what makes graph depth heterogeneous (see the module haddock and
 'depthHistogram'). 'labTrendComp' is wired for all 180 lab ids per patient
 regardless (matching the spec's per-patient instance count), but only
 chains to the previous lab id within a segment of this length; every
 @cap@'th lab id starts a fresh segment. 180 is exactly divisible by every
 value in @[1, 5]@, so every patient's 180 'labTrendComp' instances split
 into whole segments with no remainder.
-}
labTrendChainCap :: PatId -> Int
labTrendChainCap p = 1 + fromIntegral (p `mod` 5)

----------------------------------------------------------------------------
-- Source/sink ids and keys
----------------------------------------------------------------------------

-- | Necessarily top-level CAFs (see
-- @app\/Control\/Computations\/Demos\/Hospital\/CompDefs.hs@'s own ids for why):
-- comp bodies below reference them at wiring time, before 'initHospitalSrcs'
-- constructs the matching instances. Each literal instance name is written
-- down exactly once, right here.
adtSrcId, vitalsSrcId, labsSrcId, pharmacySrcId, notesSrcId :: TypedCompSrcId SystemSrc
adtSrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "hospital-bench-adt"
vitalsSrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "hospital-bench-vitals"
labsSrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "hospital-bench-labs"
pharmacySrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "hospital-bench-pharmacy"
notesSrcId = unsafeMkTypedCompSrcId (Proxy @SystemSrc) "hospital-bench-notes"

outSinkId :: TypedCompSinkId HashMapFlow
outSinkId = unsafeMkTypedCompSinkId (Proxy @HashMapFlow) "hospital-bench-out"

patKey :: PatId -> BS.ByteString
patKey p = BSC.pack ("p" ++ show p)

adtKey :: PatId -> Key
adtKey p = BSC.pack ("adt/p" ++ show p)

vitalValueKey, vitalUnitKey, vitalRangeKey :: (PatId, VitalId) -> Key
vitalValueKey (p, v) = BSC.pack ("vitals/value/p" ++ show p ++ "/v" ++ show v)
vitalUnitKey (p, v) = BSC.pack ("vitals/unit/p" ++ show p ++ "/v" ++ show v)
vitalRangeKey (p, v) = BSC.pack ("vitals/range/p" ++ show p ++ "/v" ++ show v)

labResultKey, labRangeKey, labSpecimenKey :: (PatId, LabId) -> Key
labResultKey (p, l) = BSC.pack ("labs/result/p" ++ show p ++ "/l" ++ show l)
labRangeKey (p, l) = BSC.pack ("labs/range/p" ++ show p ++ "/l" ++ show l)
labSpecimenKey (p, l) = BSC.pack ("labs/specimen/p" ++ show p ++ "/l" ++ show l)

medOrderKey, medDrugKey :: (PatId, MedId) -> Key
medOrderKey (p, m) = BSC.pack ("pharmacy/order/p" ++ show p ++ "/m" ++ show m)
medDrugKey (p, m) = BSC.pack ("pharmacy/drug/p" ++ show p ++ "/m" ++ show m)

noteTextKey, noteAuthorKey :: (PatId, NoteId) -> Key
noteTextKey (p, n) = BSC.pack ("notes/text/p" ++ show p ++ "/n" ++ show n)
noteAuthorKey (p, n) = BSC.pack ("notes/author/p" ++ show p ++ "/n" ++ show n)

-- | No source is pre-populated (see 'hospitalBenchMain' -- unlike the
-- existing benchmark's 300-key @make_kv@ mirror, seeding the ~1.6M distinct
-- keys this graph could touch would itself dominate startup time for no
-- benefit: every comp body only needs /some/ deterministic value per key,
-- not a semantically meaningful one). Every read therefore resolves via
-- this fallback the first time -- a short deterministic slice of the key
-- itself, so results still vary across keys and still change when
-- 'Control.Computations.Demos.Bench.SystemSrc.sysInsert' overwrites one
-- during the live-update phase.
valOf :: Key -> Maybe Val -> Val
valOf key = fromMaybe (BS.take 8 key)

-- | Fold a fetched byte string into a 'Word64', cheaply and deterministically
-- content-dependent -- so a live-update mutation that changes a value's
-- bytes changes every comp result derived from it, not just its length.
-- Mirrors the existing benchmark's own "combine into one wrapping Word64"
-- style (see @Bench.Main@'s @level0Body@, @base + i + d@), but folds bytes
-- directly rather than parsing decimal text: this graph's fallback values
-- ('valOf') are arbitrary byte slices of a key, not necessarily valid
-- number text, so a text parse would silently read as 0 for almost every
-- read here.
valWord64 :: Val -> Word64
valWord64 = BS.foldl' (\acc w -> acc * 31 + fromIntegral w) 0

----------------------------------------------------------------------------
-- Comp defs
----------------------------------------------------------------------------

-- | Result is 'packWord32Pair' of @(ward, admission-length)@ -- see the
-- module haddock's "Memory" section for why a packed 'Word64' rather than
-- an @(Int, Int)@ tuple. Needs @offsets@ (see 'wardOffsets') to place a
-- patient in a ward now that ward size is no longer a fixed 50 (see the
-- "Scale" section).
admissionCompDef :: [Int] -> CompDef PatId Word64
admissionCompDef offsets = defineComp "admissionComp" fullCaching $ \p -> do
  mval <- compSrcReq adtSrcId (SystemLookupReq (adtKey p))
  let v = valOf (adtKey p) mval
  pure (packWord32Pair (wardOfWith offsets p) (fromIntegral (BS.length v)))

-- | Result folds the 3 fetched values into one wrapping 'Word64' (via
-- 'valWord64') rather than caching the @(Val, Val, Val)@ triple itself --
-- see the module haddock's "Memory" section. The 3 reads themselves are
-- untouched: still one applicative batch against 'vitalsSrcId'.
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

vitalWindowCompDef :: Comp PatSubKey Word64 -> CompDef PatSubKey Word64
vitalWindowCompDef vitalC = defineComp "vitalWindowComp" fullCaching $ \pw -> do
  let p = patOf pw; w = subOf pw :: WindowId
      vitalsPerWindow' = fromIntegral vitalsPerWindow :: VitalId
      vitalIds = [w * vitalsPerWindow' .. w * vitalsPerWindow' + vitalsPerWindow' - 1]
  readings <- traverse (\v -> evalCompOrFail vitalC (mkPatSubKey p v)) vitalIds
  pure (sum readings)

-- | Result folds the 3 fetched values into one wrapping 'Word64', same as
-- 'vitalCompDef' -- see the module haddock's "Memory" section.
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

-- | Wired via 'defineRecursiveComp' (self-referential: see the module
-- haddock and 'labTrendChainCap' for the segment-reset scheme that keeps
-- 180 instances per patient while bounding chain depth to at most 5). Both
-- branches read 'labResultComp' (preserving the original's dependency
-- edge in both cases), only the recursive read of 'labTrendComp' itself is
-- conditional on not starting a fresh segment.
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

-- | Compares the two medication orders' /combined/ packed values directly
-- (rather than re-deriving and comparing byte lengths, the way the
-- pre-packing version did) now that 'medOrderComp' hands back one 'Word64'
-- instead of a @(Val, Val)@ pair to pull lengths out of.
interactionCompDef :: Comp PatSubKey Word64 -> CompDef PatSubKey Bool
interactionCompDef medOrderC = defineComp "interactionComp" fullCaching $ \pi_ -> do
  let p = patOf pi_; i = subOf pi_ :: InteractionId
      (m1, m2) = allMedPairs !! fromIntegral i
  r1 <- evalCompOrFail medOrderC (mkPatSubKey p m1)
  r2 <- evalCompOrFail medOrderC (mkPatSubKey p m2)
  pure (r1 == r2)

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

riskScoreCompDef
  :: Comp PatSubKey Word64
  -> Comp PatSubKey Word64
  -> Comp PatSubKey Bool
  -> CompDef PatId Word64
riskScoreCompDef vitalWindowC labTrendC interactionC = defineComp "riskScoreComp" fullCaching $ \p -> do
  windows <- traverse (\w -> evalCompOrFail vitalWindowC (mkPatSubKey p w)) [0 .. fromIntegral windowsPerPatient - 1]
  trends <- traverse (\l -> evalCompOrFail labTrendC (mkPatSubKey p l)) [0 .. fromIntegral labsPerPatient - 1]
  interactions <- traverse (\i -> evalCompOrFail interactionC (mkPatSubKey p i)) [0 .. fromIntegral interactionsPerPatient - 1]
  pure (sum windows + sum trends + fromIntegral (length (filter id interactions)))

-- | The one deliberately cross-system batch: one key from each of the five
-- sources, combined with the three patient-level rollups via
-- @ApplicativeDo@ into a single 8-leaf 'CompReqCombined' batch (see the
-- module haddock). Also the graph's only sink write, one per patient.
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

wardCensusCompDef :: Comp PatId Word64 -> [Int] -> CompDef WardId Word64
wardCensusCompDef admissionC offsets = defineComp "wardCensusComp" fullCaching $ \w -> do
  admissions <- traverse (evalCompOrFail admissionC) (patientsOfWard offsets w)
  pure (fromIntegral (length admissions))

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

-- | Fans in over 'patientSummaryComp' (not directly over 'riskScoreComp')
-- so 'patientSummaryComp' actually has a caller -- nothing else in this
-- graph evaluates it otherwise, and an un-evaluated comp contributes
-- nothing to the achieved instance count regardless of how it's wired. This
-- also matches the real shape of "candidates for transfer": a roll-up over
-- each patient's already-computed summary, not a re-derivation from raw
-- risk scores.
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

-- | Wires every comp def above bottom-up: ~18 wired 'Comp's total (the
-- number of /distinct/ named computations -- the ~976,000 achieved
-- instances come from calling these repeatedly at different parameters via
-- 'evalCompOrFail', exactly as in the existing benchmark's own 50-def
-- graph).
wireHospitalComps :: Int -> Int -> CompWireM (Comp () ())
wireHospitalComps patientCount wardCount = do
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
-- Depth distribution
----------------------------------------------------------------------------

{- | The exact instance count at each dependency level (source = level 0;
 @root@ is always the deepest level), computed analytically from the
 graph's known, fully deterministic shape -- not sampled at runtime. This is
 possible (and, being exact rather than sampled, preferable to instrumenting
 the engine) precisely because every comp's level is pinned down by
 @p \`mod\` 5@ alone: level 1 is every comp reading a source directly
 (admission, vital, labResult, medOrder, note); labTrendComp's level is
 @2 + (l \`mod\` cap)@, landing in @[2, 6]@; riskScore is @cap + 2@;
 patientSummary\/patientAlert are @cap + 3@; a ward's wardRiskBoard is
 @1 + max(cap + 3)@ over that ward's own patients (at the default scale,
 and at every scale point this module's own re-measurement uses, every
 ward's patient block is a multiple of 5 wide and 5-aligned, so it always
 contains a patient with @cap = 5@ and this is always 9 -- see the module
 haddock's "Scale" section for why a much smaller, oddly-sized ward could in
 principle contain a lower max @cap@ instead); hospital dashboard is
 @1 + max@ of every ward's wardRiskBoard level (and the two fixed-level-2
 ward rollups); transferCandidates is @1 + max@ of every patientSummary
 level (and admission's fixed level 1); root is @1 + max@ of dashboard and
 transferCandidates.

 Summed per patient this is exactly __976__: 1 (admission) + 250 (vital) +
 50 (vitalWindow) + 180 (labResult) + 180 (labTrend) + 18 (medOrder) + 153
 (interaction) + 140 (note) + 1 (noteDigest) + 1 (riskScore) + 1
 (patientSummary) + 1 (patientAlert) = 976. Plus 3 per ward (census,
 occupancy, risk board) and 3 total (dashboard, transfer candidates, root).
-}
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
  -- transferCandidates depends on patientSummary (level cap+3, max over all
  -- patients) and admission (level 1); 1 + max(1, maxCap + 3) simplifies to
  -- maxCap + 4 unconditionally since maxCap >= 1 makes maxCap + 4 >= 5 > 2.
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
-- Flow wiring
----------------------------------------------------------------------------

data HospitalSrcs = HospitalSrcs
  { hsrc_adt :: SystemSrc
  , hsrc_vitals :: SystemSrc
  , hsrc_labs :: SystemSrc
  , hsrc_pharmacy :: SystemSrc
  , hsrc_notes :: SystemSrc
  }

namedSrcs :: HospitalSrcs -> [(String, SystemSrc)]
namedSrcs srcs =
  [ ("adt", hsrc_adt srcs)
  , ("vitals", hsrc_vitals srcs)
  , ("labs", hsrc_labs srcs)
  , ("pharmacy", hsrc_pharmacy srcs)
  , ("notes", hsrc_notes srcs)
  ]

initHospitalSrcs :: Int -> IO HospitalSrcs
initHospitalSrcs latencyUs =
  HospitalSrcs
    <$> initSystemSrc (instTextFromTypedCompSrcId adtSrcId) latencyUs
    <*> initSystemSrc (instTextFromTypedCompSrcId vitalsSrcId) latencyUs
    <*> initSystemSrc (instTextFromTypedCompSrcId labsSrcId) latencyUs
    <*> initSystemSrc (instTextFromTypedCompSrcId pharmacySrcId) latencyUs
    <*> initSystemSrc (instTextFromTypedCompSrcId notesSrcId) latencyUs

totalSystemCallCount :: HospitalSrcs -> IO Int
totalSystemCallCount srcs = sum <$> traverse (sysCallCount . snd) (namedSrcs srcs)

-- | The bundling counterpart of 'totalSystemCallCount': total
-- 'compSrcExecuteBatch' round trips across all five sources -- see
-- 'sysBatchCallCount'. @totalSystemCallCount / totalSystemBatchCallCount@
-- is the round-trip reduction this feature buys, reportable directly rather
-- than only inferred from timing.
totalSystemBatchCallCount :: HospitalSrcs -> IO Int
totalSystemBatchCallCount srcs = sum <$> traverse (sysBatchCallCount . snd) (namedSrcs srcs)

----------------------------------------------------------------------------
-- Rerun-heavy live phase (phase 3): mutate many source keys in one go, so
-- the incremental engine's rerun path -- otherwise exercised by only 8
-- reruns in phase 2 -- gets exercised at a scale worth measuring. See the
-- module haddock and 'hospitalBenchMain'\'s own phase-3 comment.
----------------------------------------------------------------------------

-- | Default @HOSPITAL_BENCH_RERUN_KEYS@: keys mutated per phase-3 round.
-- Chosen empirically at scale 1.0 (1000 patients): 400 keys, spread via
-- 'rerunMutationTarget' across all five sources and (thanks to the 9973
-- stride) the full patient\/ward range, reproducibly produced 3,069 reruns
-- (measured via 'sysInsertBatch', see that function's own haddock for why an
-- earlier, unbatched version of this phase gave a *nondeterministic* count
-- here instead) -- roughly 380x phase 2's 8-computation cascade, i.e. a
-- genuinely \"rerun-heavy\" round -- in under a second (~0.8 s measured),
-- alongside a ~15-20 s cold eval. Small enough to leave the total benchmark
-- runtime cold-eval-dominated (this is a rerun-path microbenchmark bolted
-- onto the existing cold-eval benchmark, not a replacement for it); large
-- enough that the reruns aren't swamped by fixed per-round overhead (settle
-- polling, 'getCurrentTime' calls). Reruns scale roughly linearly with key
-- count in this range (measured: 100 keys -> 807 reruns, 700 -> 5,322,
-- 1000 -> 7,578 -- all ~7.6-8.1 reruns\/key), so this default is a runtime
-- choice, not a ceiling. 'HOSPITAL_BENCH_RERUN_LOOPS' exists precisely so a
-- profiling run can push the balance further towards reruns without
-- changing this default.
defaultRerunKeys :: Int
defaultRerunKeys = 400

-- | Distinct payload written to every phase-3 mutation target, keyed by a
-- globally unique index @n@ (see 'rerunMutationTarget's caller, which never
-- reuses an @n@ -- not even across repeated rounds via
-- @HOSPITAL_BENCH_RERUN_LOOPS@) so no two mutations ever collide on content,
-- and every mutation is a genuinely fresh value even when it lands on a key
-- an earlier round already touched. Deliberately far longer than every
-- 'valOf' fallback's 8 bytes, with genuinely different bytes per @n@, so
-- both length-derived results ('admissionComp') and content-derived results
-- (every 'valWord64' fold) actually change -- not merely the version tag the
-- engine's own dependency matching keys off of. Mirrors the existing
-- single-key phase's own 40-byte-vs-8-byte trick (see 'hospitalBenchMain'\'s
-- phase-2 comment).
rerunMutationVal :: Int -> Val
rerunMutationVal n = BSC.pack ("rerun-" ++ show n ++ "-") <> BSC.replicate 32 'r'

{- | The @n@-th (0-indexed, globally unique across every phase-3 round)
 mutation target: cycles through the five sources every 5 steps and picks a
 patient via a 9973 stride -- prime, and therefore coprime to every
 'scaledPatientCount' this module's scale knob can produce (always
 @round (1000 * scale)@, never a multiple of the prime 9973 at any scale this
 benchmark is run at) -- rather than @n \`mod\` patientCount@, so a batch far
 smaller than @patientCount@ still spreads across the /entire/ patient id
 range, and therefore across every ward, not just a low contiguous prefix.

 Sub-ids (vital\/lab\/med\/note id) advance once per lap through the five
 sources, so each source's /first/ hit in a batch lands on sub-id 0 -- the
 same sub-id 'patientSummaryComp' itself reads directly in its cross-system
 batch (vitals\/labs\/pharmacy\/notes at sub-id 0, see that comp's own
 definition) -- giving those particular hits a second, independent
 propagation path on top of the indirect chain the module haddock describes
 (vitalComp -> vitalWindowComp -> riskScoreComp -> ...).
-}
rerunMutationTarget :: HospitalSrcs -> Int -> Int -> (SystemSrc, Key)
rerunMutationTarget srcs patientCount n =
  case n `mod` 5 of
    0 -> (hsrc_adt srcs, adtKey p)
    1 -> (hsrc_vitals srcs, vitalValueKey (p, subId `mod` fromIntegral vitalsPerPatient))
    2 -> (hsrc_labs srcs, labResultKey (p, subId `mod` fromIntegral labsPerPatient))
    3 -> (hsrc_pharmacy srcs, medOrderKey (p, subId `mod` fromIntegral medsPerPatient))
    _ -> (hsrc_notes srcs, noteTextKey (p, subId `mod` fromIntegral notesPerPatient))
 where
  p = fromIntegral ((n * 9973) `mod` patientCount) :: PatId
  subId = fromIntegral (n `div` 5) :: Word32

{- | Registers all five sources plus the sink, and sets the registry's
 'CompFlowConcurrency' width -- done here, inside the very
 @withRegisteredFlows@ callback 'hospitalBenchDriver' (mirroring
 'Control.Computations.CompEngine.Driver.compDriver'') hands the registry
 to, so no driver forking is needed to reach it (see
 'CompFlowRegistry'\'s own haddock for why that's the intended access
 point).
-}
withHospitalFlows :: HospitalSrcs -> HashMapFlow -> Int -> CompFlowRegistry -> IO () -> IO ()
withHospitalFlows srcs sink width reg action = do
  setCompFlowConcurrency reg (mkCompFlowConcurrency width)
  forM_ (namedSrcs srcs) (registerCompSrc reg . snd)
  registerCompSink reg sink
  action

----------------------------------------------------------------------------
-- Counting driver (duplicated from Bench.Main's benchCompDriver rather than
-- reused: that function is Bench.Main-internal, not exported, and the
-- existing benchmark must stay exactly as it is -- see the module haddock).
-- Both this and Bench.Main's copy are written entirely against the public
-- "Control.Computations.CompEngine" facade, so the duplication is a plain
-- copy, not a fork of anything internal.
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

hospitalBenchDriver
  :: IORef Int
  -> TVar (Option RunStats)
  -> (CompFlowRegistry -> IO () -> IO ())
  -> CompWireM (Comp () ())
  -> IO ()
hospitalBenchDriver counterRef runVar withRegisteredFlows wireComps = do
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

-- | The patient count at a given scale: @max 1 (round (1000 * scale))@ --
-- see the module haddock's "Scale" section.
scaledPatientCount :: Double -> Int
scaledPatientCount scale = max 1 (round (fromIntegral basePatientCount * scale :: Double))

-- | The ward count at a given scale: @max 1 (round (20 * scale))@, clamped
-- to never exceed @patientCount@ (so every ward gets at least 1 patient --
-- see 'wardSizes') -- see the module haddock's "Scale" section for why this
-- (unlike the original fixed-50-per-ward scheme) makes the scale knob
-- genuinely continuous.
scaledWardCount :: Double -> Int -> Int
scaledWardCount scale patientCount =
  min patientCount (max 1 (round (fromIntegral baseWardCount * scale :: Double)))

----------------------------------------------------------------------------
-- Memory measurement (duplicated from Bench.Main's getRssMb for the same
-- reason as hospitalBenchDriver above)
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

hospitalBenchMain :: IO ()
hospitalBenchMain = do
  scale <- readEnvDouble "HOSPITAL_BENCH_SCALE" 1.0
  latencyUs <- readEnvIntAtLeast 0 "HOSPITAL_BENCH_SRC_LATENCY_US" 0
  width <- readEnvIntAtLeast 1 "HOSPITAL_BENCH_CONCURRENCY" 1
  let patientCount = scaledPatientCount scale
      wardCount = scaledWardCount scale patientCount
      histogram = depthHistogram wardCount patientCount
      targetInstances = sum (Map.elems histogram)

  putStrLn "=== bench: hospital pipeline (applicative-batch, concurrent-source benchmark) ==="
  printf "HOSPITAL_BENCH_SCALE=%.4f HOSPITAL_BENCH_SRC_LATENCY_US=%d HOSPITAL_BENCH_CONCURRENCY=%d\n" scale latencyUs width
  printf
    "patients: %d, wards: %d (avg %.1f patients/ward), target instances (analytic): %d\n"
    patientCount
    wardCount
    (fromIntegral patientCount / fromIntegral wardCount :: Double)
    targetInstances
  putStrLn "depth distribution (level -> instance count):"
  forM_ (Map.toAscList histogram) $ \(lvl, n) -> printf "  level %2d: %d\n" lvl n
  putStrLn ""

  srcs <- initHospitalSrcs latencyUs
  sink <- initHashMapFlow (instTextFromTypedCompSinkId outSinkId)

  runVar <- newTVarIO None
  counterRef <- newIORef 0
  t0 <- getCurrentTime
  allocated0 <- allocated_bytes <$> getRTSStats
  engineHandle <-
    async
      ( hospitalBenchDriver
          counterRef
          runVar
          (withHospitalFlows srcs sink width)
          (wireHospitalComps patientCount wardCount)
      )

  -- Cold settle: same reasoning as Bench.Main -- the entire initial
  -- evaluation runs synchronously inside Impl.startCompEngine, so the first
  -- posted RunStats (run == 1) is exactly the moment cold eval finished.
  rs1 <- waitForRunAtLeast runVar 1
  tCold <- getCurrentTime
  coldReruns <- readIORef counterRef
  rssCold <- getRssMb
  allocatedCold <- allocated_bytes <$> getRTSStats
  let coldWallTime = realToFrac (diffUTCTime tCold t0) :: Double
      -- 'allocated_bytes' is GHC's own running total of bytes allocated
      -- since process start -- program-driven, not GC-driven, so for a fixed
      -- program and input it should be stable run to run, in a way wall time
      -- on this machine has proven not to be (same-session, same-code wall
      -- time has been observed to vary roughly 2x run to run, well above the
      -- 5-15% effect sizes this benchmark exists to detect). The delta
      -- across a phase boundary is that phase's own allocation.
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

  -- Settle-race workaround: see the module haddock and Bench.Main's own
  -- ~40-line comment on its equivalent step. There is no pre-population
  -- backlog to drain here (unlike Bench.Main's 300 keys), but the race in
  -- how compDriver'-shaped loops post RunStats is a property of the driver,
  -- not of whether there happens to be a backlog -- so the same defensive
  -- wait is still required before timing the live update below.
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

  -- 2. Live incremental update: mutate one vitals key and time until the
  -- driver's next propagation round fully settles.
  tBeforeMutate <- getCurrentTime
  allocatedPreLive <- allocated_bytes <$> getRTSStats
  -- 40 bytes, deliberately far from every 8-byte 'valOf' fallback default so
  -- the content-derived numbers this graph computes ('valWord64' folds,
  -- specifically) actually change rather than propagating one hop and
  -- stopping on an unchanged cached hash -- see the module's live-update
  -- reasoning.
  sysInsert (hsrc_vitals srcs) (vitalValueKey (0, 0)) (BSC.replicate 40 'x')
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

  -- 3. Rerun-heavy live update: mutate many source keys in one go -- spread
  -- across patients and across all five sources via 'rerunMutationTarget' --
  -- and time until the *entire* resulting cascade fully settles. This is
  -- something phase 2 above cannot stand in for: one vitals key cascades
  -- vitalComp -> vitalWindowComp -> riskScoreComp ->
  -- patientSummaryComp\/patientAlertComp -> that patient's ward comps -> the
  -- dashboard (8 reruns total, see the module haddock's "Why" motivation),
  -- so a few hundred keys spread the same way produces a rerun count two to
  -- three orders of magnitude larger -- enough to actually measure the
  -- incremental engine's rerun path rather than gesture at it.
  --
  -- Runs unconditionally at a nonzero default, unlike
  -- "Control.Computations.Demos.Bench.Main"'s off-by-default
  -- @PERSIST_BENCH_LIVE_LOOPS@ diagnostic: this phase (not phase 2) is what
  -- future rerun-path optimisation work is meant to be judged against, so it
  -- has to run -- and be reported -- on every plain @HOSPITAL_BENCH=1 stack
  -- bench@ invocation. Set @HOSPITAL_BENCH_RERUN_KEYS=0@ to disable it
  -- entirely, e.g. to isolate phases 1-2's own state-lock cost under
  -- @COMP_ENGINE_LOCK_STATS@ by differencing two whole-process runs (see
  -- docs/benchmark-notes.md) -- the accumulate-to-close-only nature of that
  -- instrumentation is exactly why this phase must exist as an on/off
  -- switch, not just a "does it exist" question.
  --
  -- @HOSPITAL_BENCH_RERUN_LOOPS@ (default 1, i.e. one round) repeats the
  -- whole mutate-and-settle round, mirroring @PERSIST_BENCH_LIVE_LOOPS@, so
  -- a profile taken over the whole process can be made to skew arbitrarily
  -- far towards rerun cost rather than cold eval.
  --
  -- Settle discipline: starting from @rs_run rs3 + 1@ needs no timeout
  -- fallback, unlike the cold-drain wait after phase 1 -- 'rs3' is itself
  -- 'waitForFullSettle's postcondition (genuinely 0 leftover stale caps)
  -- from phase 2's *real* mutation, so with 'rcif_emptyChangesMode' = 'Block'
  -- the engine is provably parked at run 'rs_run rs3' by the time we get
  -- here, exactly the situation phase 2's own mutation relied on relative to
  -- 'rs2' (see that comment above).
  rerunKeysPerRound <- readEnvIntAtLeast 0 "HOSPITAL_BENCH_RERUN_KEYS" defaultRerunKeys
  rerunRounds <- readEnvIntAtLeast 1 "HOSPITAL_BENCH_RERUN_LOOPS" 1
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
      -- One 'sysInsertBatch' call, not @rerunKeysPerRound@ separate
      -- 'sysInsert' calls -- see that function's own haddock for why a
      -- multi-key "in one go" mutation *must* land in a single STM
      -- transaction to avoid the engine's driver thread observing (and
      -- fully settling on) a partial batch before the rest of this round's
      -- writes have even happened.
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
    putStrLn "--- 3. rerun-heavy live update (multi-key, HOSPITAL_BENCH_RERUN_KEYS/_LOOPS) ---"
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
      -- The comparable-across-machines number: 'allocated_bytes' is
      -- program-driven (GHC's own running allocation counter), unlike
      -- us\/rerun above, which is wall-clock-derived and therefore inherits
      -- whatever noise this machine's scheduler, other processes, and GC
      -- pause timing contribute on top of the work actually done.
      printf
        "allocated_bytes/rerun: %.1f B/rerun\n"
        (fromIntegral rerunAllocated / fromIntegral rerunReruns :: Double)

  cancel engineHandle
 where
  hashMapKeys = HashMap.keys
