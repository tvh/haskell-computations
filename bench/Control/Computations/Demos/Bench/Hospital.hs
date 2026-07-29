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
 'HashMapFlow' is the sink. 1,000 patients across 20 wards (50 patients per
 ward), scaled by @HOSPITAL_BENCH_SCALE@, target
 __~976 computation instances per patient__ (see 'depthHistogram'\'s haddock
 for the exact per-comp breakdown and why it sums to precisely 976) --
 ~976,000 total at the default scale, the same order as the existing
 benchmark's ~1.14M.

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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time.Clock
import GHC.Stats
import System.Environment (lookupEnv)
import System.Posix.Process (getProcessID)
import System.Process (readProcess)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

----------------------------------------------------------------------------
-- Graph shape
----------------------------------------------------------------------------

type PatId = Int
type WardId = Int
type VitalId = Int
type WindowId = Int
type LabId = Int
type MedId = Int
type InteractionId = Int
type NoteId = Int

-- | Fixed at 50 regardless of scale -- only the number of wards scales (see
-- 'scaledWardCount'), which scales the patient count (@wardCount * 50@)
-- along with it.
patientsPerWard :: Int
patientsPerWard = 50

baseWardCount :: Int
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
allMedPairs = [(a, b) | a <- [0 .. medsPerPatient - 1], b <- [a + 1 .. medsPerPatient - 1]]

interactionsPerPatient :: Int
interactionsPerPatient = length allMedPairs

wardOf :: PatId -> WardId
wardOf p = p `div` patientsPerWard

patientsOfWard :: WardId -> [PatId]
patientsOfWard w = [w * patientsPerWard .. w * patientsPerWard + patientsPerWard - 1]

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
labTrendChainCap p = 1 + (p `mod` 5)

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

----------------------------------------------------------------------------
-- Comp defs
----------------------------------------------------------------------------

admissionCompDef :: CompDef PatId (Int, Int)
admissionCompDef = defineComp "admissionComp" fullCaching $ \p -> do
  mval <- compSrcReq adtSrcId (SystemLookupReq (adtKey p))
  let v = valOf (adtKey p) mval
  pure (wardOf p, BS.length v)

vitalCompDef :: CompDef (PatId, VitalId) (Val, Val, Val)
vitalCompDef = defineComp "vitalComp" fullCaching $ \pv -> do
  value <- compSrcReq vitalsSrcId (SystemLookupReq (vitalValueKey pv))
  unit <- compSrcReq vitalsSrcId (SystemLookupReq (vitalUnitKey pv))
  range <- compSrcReq vitalsSrcId (SystemLookupReq (vitalRangeKey pv))
  pure
    ( valOf (vitalValueKey pv) value
    , valOf (vitalUnitKey pv) unit
    , valOf (vitalRangeKey pv) range
    )

vitalWindowCompDef :: Comp (PatId, VitalId) (Val, Val, Val) -> CompDef (PatId, WindowId) Int
vitalWindowCompDef vitalC = defineComp "vitalWindowComp" fullCaching $ \(p, w) -> do
  let vitalIds = [w * vitalsPerWindow .. w * vitalsPerWindow + vitalsPerWindow - 1]
  readings <- traverse (\v -> evalCompOrFail vitalC (p, v)) vitalIds
  pure (sum [BS.length val | (val, _, _) <- readings])

labResultCompDef :: CompDef (PatId, LabId) (Val, Val, Val)
labResultCompDef = defineComp "labResultComp" fullCaching $ \pl -> do
  result <- compSrcReq labsSrcId (SystemLookupReq (labResultKey pl))
  range <- compSrcReq labsSrcId (SystemLookupReq (labRangeKey pl))
  specimen <- compSrcReq labsSrcId (SystemLookupReq (labSpecimenKey pl))
  pure
    ( valOf (labResultKey pl) result
    , valOf (labRangeKey pl) range
    , valOf (labSpecimenKey pl) specimen
    )

-- | Wired via 'defineRecursiveComp' (self-referential: see the module
-- haddock and 'labTrendChainCap' for the segment-reset scheme that keeps
-- 180 instances per patient while bounding chain depth to at most 5).
labTrendCompDef
  :: Comp (PatId, LabId) (Val, Val, Val) -> Comp (PatId, LabId) Int -> CompDef (PatId, LabId) Int
labTrendCompDef labResultC labTrendC = defineComp "labTrendComp" fullCaching $ \(p, l) ->
  let cap = labTrendChainCap p
      s = l `mod` cap
   in if s == 0
        then do
          (resultVal, _, _) <- evalCompOrFail labResultC (p, l)
          pure (BS.length resultVal)
        else do
          (resultVal, _, _) <- evalCompOrFail labResultC (p, l)
          prev <- evalCompOrFail labTrendC (p, l - 1)
          pure (prev + BS.length resultVal)

medOrderCompDef :: CompDef (PatId, MedId) (Val, Val)
medOrderCompDef = defineComp "medOrderComp" fullCaching $ \pm -> do
  order <- compSrcReq pharmacySrcId (SystemLookupReq (medOrderKey pm))
  drug <- compSrcReq pharmacySrcId (SystemLookupReq (medDrugKey pm))
  pure (valOf (medOrderKey pm) order, valOf (medDrugKey pm) drug)

interactionCompDef :: Comp (PatId, MedId) (Val, Val) -> CompDef (PatId, InteractionId) Bool
interactionCompDef medOrderC = defineComp "interactionComp" fullCaching $ \(p, i) -> do
  let (m1, m2) = allMedPairs !! i
  (o1, d1) <- evalCompOrFail medOrderC (p, m1)
  (o2, d2) <- evalCompOrFail medOrderC (p, m2)
  pure (BS.length o1 + BS.length d1 == BS.length o2 + BS.length d2)

noteCompDef :: CompDef (PatId, NoteId) (Val, Val)
noteCompDef = defineComp "noteComp" fullCaching $ \pn -> do
  text <- compSrcReq notesSrcId (SystemLookupReq (noteTextKey pn))
  author <- compSrcReq notesSrcId (SystemLookupReq (noteAuthorKey pn))
  pure (valOf (noteTextKey pn) text, valOf (noteAuthorKey pn) author)

noteDigestCompDef :: Comp (PatId, NoteId) (Val, Val) -> CompDef PatId Int
noteDigestCompDef noteC = defineComp "noteDigestComp" fullCaching $ \p -> do
  notes <- traverse (\n -> evalCompOrFail noteC (p, n)) [0 .. notesPerPatient - 1]
  pure (sum [BS.length t + BS.length a | (t, a) <- notes])

riskScoreCompDef
  :: Comp (PatId, WindowId) Int
  -> Comp (PatId, LabId) Int
  -> Comp (PatId, InteractionId) Bool
  -> CompDef PatId Int
riskScoreCompDef vitalWindowC labTrendC interactionC = defineComp "riskScoreComp" fullCaching $ \p -> do
  windows <- traverse (\w -> evalCompOrFail vitalWindowC (p, w)) [0 .. windowsPerPatient - 1]
  trends <- traverse (\l -> evalCompOrFail labTrendC (p, l)) [0 .. labsPerPatient - 1]
  interactions <- traverse (\i -> evalCompOrFail interactionC (p, i)) [0 .. interactionsPerPatient - 1]
  pure (sum windows + sum trends + length (filter id interactions))

-- | The one deliberately cross-system batch: one key from each of the five
-- sources, combined with the three patient-level rollups via
-- @ApplicativeDo@ into a single 8-leaf 'CompReqCombined' batch (see the
-- module haddock). Also the graph's only sink write, one per patient.
patientSummaryCompDef
  :: Comp PatId Int -> Comp PatId (Int, Int) -> Comp PatId Int -> CompDef PatId Int
patientSummaryCompDef riskScoreC admissionC noteDigestC =
  defineComp "patientSummaryComp" fullCaching $ \p -> do
    risk <- evalCompOrFail riskScoreC p
    (ward, _admitLen) <- evalCompOrFail admissionC p
    noteDigest <- evalCompOrFail noteDigestC p
    adtVal <- compSrcReq adtSrcId (SystemLookupReq (adtKey p))
    vitalsVal <- compSrcReq vitalsSrcId (SystemLookupReq (vitalValueKey (p, 0)))
    labsVal <- compSrcReq labsSrcId (SystemLookupReq (labResultKey (p, 0)))
    pharmacyVal <- compSrcReq pharmacySrcId (SystemLookupReq (medOrderKey (p, 0)))
    notesVal <- compSrcReq notesSrcId (SystemLookupReq (noteTextKey (p, 0)))
    let crossLen =
          BS.length (valOf (adtKey p) adtVal)
            + BS.length (valOf (vitalValueKey (p, 0)) vitalsVal)
            + BS.length (valOf (labResultKey (p, 0)) labsVal)
            + BS.length (valOf (medOrderKey (p, 0)) pharmacyVal)
            + BS.length (valOf (noteTextKey (p, 0)) notesVal)
        summary = risk + ward + noteDigest + crossLen
    void $ compSinkReq outSinkId (HashMapStoreReq (patKey p) (BSC.pack (show summary)))
    pure summary

patientAlertCompDef :: Comp PatId Int -> Comp PatId (Int, Int) -> CompDef PatId Bool
patientAlertCompDef riskScoreC admissionC = defineComp "patientAlertComp" fullCaching $ \p -> do
  risk <- evalCompOrFail riskScoreC p
  (ward, _) <- evalCompOrFail admissionC p
  pure (risk `mod` 7 == ward `mod` 7)

wardCensusCompDef :: Comp PatId (Int, Int) -> CompDef WardId Int
wardCensusCompDef admissionC = defineComp "wardCensusComp" fullCaching $ \w -> do
  admissions <- traverse (evalCompOrFail admissionC) (patientsOfWard w)
  pure (length admissions)

wardOccupancyCompDef :: Comp PatId (Int, Int) -> CompDef WardId Int
wardOccupancyCompDef admissionC = defineComp "wardOccupancyComp" fullCaching $ \w -> do
  admissions <- traverse (evalCompOrFail admissionC) (patientsOfWard w)
  pure (sum [len | (_, len) <- admissions])

wardRiskBoardCompDef :: Comp PatId Bool -> CompDef WardId Int
wardRiskBoardCompDef patientAlertC = defineComp "wardRiskBoardComp" fullCaching $ \w -> do
  alerts <- traverse (evalCompOrFail patientAlertC) (patientsOfWard w)
  pure (length (filter id alerts))

hospitalDashboardCompDef
  :: Comp WardId Int -> Comp WardId Int -> Comp WardId Int -> Int -> CompDef () Int
hospitalDashboardCompDef wardCensusC wardRiskBoardC wardOccupancyC wardCount =
  defineComp "hospitalDashboardComp" fullCaching $ \() -> do
    censuses <- traverse (evalCompOrFail wardCensusC) [0 .. wardCount - 1]
    riskBoards <- traverse (evalCompOrFail wardRiskBoardC) [0 .. wardCount - 1]
    occupancies <- traverse (evalCompOrFail wardOccupancyC) [0 .. wardCount - 1]
    pure (sum censuses + sum riskBoards + sum occupancies)

-- | Fans in over 'patientSummaryComp' (not directly over 'riskScoreComp')
-- so 'patientSummaryComp' actually has a caller -- nothing else in this
-- graph evaluates it otherwise, and an un-evaluated comp contributes
-- nothing to the achieved instance count regardless of how it's wired. This
-- also matches the real shape of "candidates for transfer": a roll-up over
-- each patient's already-computed summary, not a re-derivation from raw
-- risk scores.
transferCandidatesCompDef :: Comp PatId Int -> Comp PatId (Int, Int) -> Int -> CompDef () Int
transferCandidatesCompDef patientSummaryC admissionC patientCount =
  defineComp "transferCandidatesComp" fullCaching $ \() -> do
    summaries <- traverse (evalCompOrFail patientSummaryC) [0 .. patientCount - 1]
    admissions <- traverse (evalCompOrFail admissionC) [0 .. patientCount - 1]
    pure (length [() | (s, (ward, _)) <- zip summaries admissions, s > ward])

rootCompDef :: Comp () Int -> Comp () Int -> CompDef () ()
rootCompDef hospitalDashboardC transferCandidatesC = defineComp "root" fullCaching $ \() -> do
  _ <- evalCompOrFail hospitalDashboardC ()
  _ <- evalCompOrFail transferCandidatesC ()
  pure ()

-- | Wires every comp def above bottom-up: ~18 wired 'Comp's total (the
-- number of /distinct/ named computations -- the ~976,000 achieved
-- instances come from calling these repeatedly at different parameters via
-- 'evalCompOrFail', exactly as in the existing benchmark's own 50-def
-- graph).
wireHospitalComps :: Int -> CompWireM (Comp () ())
wireHospitalComps wardCount = do
  let patientCount = wardCount * patientsPerWard
  admissionC <- wireComp admissionCompDef
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
  wardCensusC <- wireComp (wardCensusCompDef admissionC)
  wardOccupancyC <- wireComp (wardOccupancyCompDef admissionC)
  wardRiskBoardC <- wireComp (wardRiskBoardCompDef patientAlertC)
  hospitalDashboardC <-
    wireComp (hospitalDashboardCompDef wardCensusC wardRiskBoardC wardOccupancyC wardCount)
  transferCandidatesC <- wireComp (transferCandidatesCompDef patientSummaryC admissionC patientCount)
  wireComp (rootCompDef hospitalDashboardC transferCandidatesC)

----------------------------------------------------------------------------
-- Depth distribution
----------------------------------------------------------------------------

{- | The exact instance count at each dependency level (source = level 0;
 @root@ is always level 11), computed analytically from the graph's known,
 fully deterministic shape -- not sampled at runtime. This is possible
 (and, being exact rather than sampled, preferable to instrumenting the
 engine) precisely because every comp's level is pinned down by @p mod 5@
 alone: level 1 is every comp reading a source directly (admission, vital,
 labResult, medOrder, note); labTrendComp's level is @2 + (l \`mod\` cap)@,
 landing in @[2, 6]@; riskScore is @cap + 2@; patientSummary\/patientAlert
 are @cap + 3@; wardRiskBoard is always 9 (every 50-patient ward block
 contains all five @cap@ values, since 50 is a multiple of 5); hospital
 dashboard is 10; root is 11.

 Summed per patient this is exactly __976__: 1 (admission) + 250 (vital) +
 50 (vitalWindow) + 180 (labResult) + 180 (labTrend) + 18 (medOrder) + 153
 (interaction) + 140 (note) + 1 (noteDigest) + 1 (riskScore) + 1
 (patientSummary) + 1 (patientAlert) = 976. Plus 3 per ward (census,
 occupancy, risk board) and 3 total (dashboard, transfer candidates, root).
-}
depthHistogram :: Int -> Map.Map Int Int
depthHistogram wardCount =
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
  patientCount = wardCount * patientsPerWard
  caps = [labTrendChainCap p | p <- [0 .. patientCount - 1]]
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
  topLevels = [(9, wardCount), (9, 1), (10, 1), (11, 1)]

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

-- | The ward count at a given scale: @max 1 (round (20 * scale))@, floored
-- at 1 (as opposed to floored at 1 per *level*, the way the existing
-- benchmark's 'Control.Computations.Demos.Bench.Main.scaledLevelSizes'
-- floors each of its ten levels independently -- there is only the one
-- count to scale here).
scaledWardCount :: Double -> Int
scaledWardCount scale = max 1 (round (fromIntegral baseWardCount * scale :: Double))

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
  let wardCount = scaledWardCount scale
      patientCount = wardCount * patientsPerWard
      histogram = depthHistogram wardCount
      targetInstances = sum (Map.elems histogram)

  putStrLn "=== bench: hospital pipeline (applicative-batch, concurrent-source benchmark) ==="
  printf "HOSPITAL_BENCH_SCALE=%.4f HOSPITAL_BENCH_SRC_LATENCY_US=%d HOSPITAL_BENCH_CONCURRENCY=%d\n" scale latencyUs width
  printf "wards: %d, patients/ward: %d, patients: %d\n" wardCount patientsPerWard patientCount
  printf "target instances (analytic): %d\n" targetInstances
  putStrLn "depth distribution (level -> instance count):"
  forM_ (Map.toAscList histogram) $ \(lvl, n) -> printf "  level %2d: %d\n" lvl n
  putStrLn ""

  srcs <- initHospitalSrcs latencyUs
  sink <- initHashMapFlow (instTextFromTypedCompSinkId outSinkId)

  runVar <- newTVarIO None
  counterRef <- newIORef 0
  t0 <- getCurrentTime
  engineHandle <-
    async
      ( hospitalBenchDriver
          counterRef
          runVar
          (withHospitalFlows srcs sink width)
          (wireHospitalComps wardCount)
      )

  -- Cold settle: same reasoning as Bench.Main -- the entire initial
  -- evaluation runs synchronously inside Impl.startCompEngine, so the first
  -- posted RunStats (run == 1) is exactly the moment cold eval finished.
  rs1 <- waitForRunAtLeast runVar 1
  tCold <- getCurrentTime
  coldReruns <- readIORef counterRef
  rssCold <- getRssMb
  let coldWallTime = realToFrac (diffUTCTime tCold t0) :: Double

  putStrLn "--- 1. cold eval ---"
  printf
    "achieved instance count: %d (target %d, %+.2f%%)\n"
    coldReruns
    targetInstances
    (100 * (fromIntegral coldReruns - fromIntegral targetInstances) / fromIntegral targetInstances :: Double)
  printf "wall time: %.3f s\n" coldWallTime
  printf "RSS after cold settle: %.1f MB\n" rssCold

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
  -- 40 bytes, deliberately far from every 8-byte 'valOf' fallback default so
  -- the length-derived numbers this graph computes (vitalWindowComp's
  -- summed lengths, in particular) actually change rather than propagating
  -- one hop and stopping on an unchanged cached hash -- see the module's
  -- live-update reasoning.
  sysInsert (hsrc_vitals srcs) (vitalValueKey (0, 0)) (BSC.replicate 40 'x')
  _rs3 <- waitForFullSettle runVar (rs_run rs2 + 1)
  tAfterMutate <- getCurrentTime
  liveReruns <- readIORef counterRef
  let liveWallTime = realToFrac (diffUTCTime tAfterMutate tBeforeMutate) :: Double
      liveRerunCount = liveReruns - preLiveReruns

  putStrLn ""
  putStrLn "--- 2. live incremental, 1 changed vitals key ---"
  printf "wall time: %.4f s\n" liveWallTime
  printf "reruns: %d\n" liveRerunCount

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

  putStrLn ""
  putStrLn "--- source calls ---"
  totalCalls <- totalSystemCallCount srcs
  printf "total source calls: %d\n" totalCalls
  forM_ (namedSrcs srcs) $ \(name, s) -> do
    calls <- sysCallCount s
    hw <- sysHighWaterMark s
    printf "  %-10s calls=%-8d concurrency high-water mark=%d\n" name calls hw

  cancel engineHandle
 where
  hashMapKeys = HashMap.keys
