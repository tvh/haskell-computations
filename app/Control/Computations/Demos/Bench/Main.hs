{- | Scale benchmark mirroring the persistence-free phases of the Rust
 @persist_bench@ example
 (@rust-computations\/crates\/computations\/examples\/persist_bench.rs@):

   1. __Cold eval__: build a synthetic 10-level, 50-definition dependency
      graph and run it to settled, measuring wall time, achieved instance
      count (comp body invocations) and RSS. This is Rust's "phase 5, cold
      restart, no persistence".
   2. __Live incremental__: with the same still-running engine, mutate
      exactly one source key and measure wall time until the next
      propagation round completes, plus the rerun count during that round.
      This is Rust's "phase 7".

 Persistence and engine optimizations are explicitly out of scope: this
 measures the reference (@fullCaching@) engine as-is, in a single process.

 Scale is configurable via the @PERSIST_BENCH_SCALE@ environment variable
 (a float multiplier on every entry of 'baseLevelSizes', each floored at 1;
 defaults to @1.0@, the ~1,000,000-instance configuration).
-}
module Control.Computations.Demos.Bench.Main (benchMain) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.FlowImpls.HashMapFlow
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Time.Clock
import Data.Word
import GHC.Stats
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessID)
import System.Process (readProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)

----------------------------------------
-- Graph shape (mirrors Rust persist_bench.rs)
----------------------------------------

-- | Number of dependency levels. Level 0 reads sources; the last level
-- ("top") writes to the sink.
levelsCount :: Int
levelsCount = 10

-- | Definitions per level.
defsPerLevel :: Int
defsPerLevel = 5

-- | Distinct source keys level-0 instances read from (@index \`mod\` srcKeys@).
srcKeys :: Word32
srcKeys = 300

-- | Instances per level, bottom (level 0) to top (level 9), at
-- @PERSIST_BENCH_SCALE=1.0@ (the default). See the Rust module docs for why
-- these are front-loaded rather than flat.
baseLevelSizes :: [Word32]
baseLevelSizes = [205000, 205000, 205000, 205000, 205000, 128945, 101885, 61090, 27060, 1000]

-- | Reads @PERSIST_BENCH_SCALE@ (defaulting to @1.0@ when unset or
-- unparseable) and returns the scaled level-size vector, each entry floored
-- at 1.
readScale :: IO Double
readScale = do
  mScale <- lookupEnv "PERSIST_BENCH_SCALE"
  pure (fromMaybe 1.0 (mScale >>= readMaybe))

scaledLevelSizes :: Double -> [Word32]
scaledLevelSizes scale = map scaleOne baseLevelSizes
 where
  scaleOne base = max 1 (round (fromIntegral base * scale :: Double))

----------------------------------------
-- Benchmark-only rerun counter
----------------------------------------

{- | 'CompM' has no sanctioned way to run 'IO' inside a computation body (no
 @liftIO@), unlike the Rust benchmark's async bodies which do a plain
 @run_counter.fetch_add@. This is a benchmark-only escape hatch via
 'unsafePerformIO', exactly as anticipated by the benchmark spec.
-}
{-# NOINLINE benchRerunCounter #-}
benchRerunCounter :: IORef Int
benchRerunCounter = unsafePerformIO (newIORef 0)

{- | Bump 'benchRerunCounter' and return @()@. Takes the (otherwise unused)
 instance parameter as an argument and forces it with 'seq': this keeps
 GHC's full-laziness float-out from turning the 'unsafePerformIO' call into
 a single shared top-level thunk that would only ever run once.
-}
{-# NOINLINE bumpRerunCounter #-}
bumpRerunCounter :: a -> ()
bumpRerunCounter x =
  x `seq` unsafePerformIO (atomicModifyIORef' benchRerunCounter (\n -> (n + 1, ())))

readRerunCounter :: IO Int
readRerunCounter = readIORef benchRerunCounter

----------------------------------------
-- Flow wiring
----------------------------------------

kvSrcId :: TypedCompSrcId HashMapFlow
kvSrcId = typedCompSrcId (Proxy @HashMapFlow) "bench-kv"

docSinkId :: TypedCompSinkId HashMapFlow
docSinkId = typedCompSinkId (Proxy @HashMapFlow) "bench-doc-sink"

withCompFlows :: HashMapFlow -> HashMapFlow -> CompFlowRegistry -> IO () -> IO ()
withCompFlows kv docSink reg action = do
  registerCompSrc reg kv
  registerCompSink reg docSink
  action

----------------------------------------
-- Graph definitions
----------------------------------------

type Level = [Comp Word32 Word64]

docKey :: Word32 -> BSC.ByteString
docKey i = BSC.pack ("doc_" ++ show i)

srcKeyFor :: Word32 -> BSC.ByteString
srcKeyFor i = BSC.pack (show (i `mod` srcKeys))

parseWord64 :: BSC.ByteString -> Maybe Word64
parseWord64 = readMaybe . BSC.unpack

-- | Level-0 def body: read source key @show (i \`mod\` srcKeys)@; base is
-- the parsed value or 0; result is @base + i + d@ (all wrapping 'Word64').
level0Body :: Word64 -> Word32 -> CompM Word64
level0Body dWord i = do
  let !_ = bumpRerunCounter i
  mval <- compSrcReq kvSrcId (HashMapLookupReq (srcKeyFor i))
  let base = fromMaybe 0 (mval >>= parseWord64)
  pure (base + fromIntegral i + dWord)

-- | Level-@L>0@ def body: fold three fan-in edges from the previous level
-- (via fixed modular-arithmetic index formulas, matching the Rust spec
-- exactly so 'Word32' wraps identically), then combine with @d@ and @i@.
-- Top-level instances additionally write their result to the doc sink.
higherLevelBody :: Level -> Word32 -> Bool -> Word64 -> Word32 -> CompM Word64
higherLevelBody prevLevel prevSize isTop dWord i = do
  let !_ = bumpRerunCounter i
      c0 = (i * 2 + 1) `mod` prevSize
      c1 = (i * 7 + 13) `mod` prevSize
      c2 = (i * 31 + 101) `mod` prevSize
      pick c = prevLevel !! fromIntegral (c `mod` fromIntegral defsPerLevel)
  r0 <- evalCompOrFail (pick c0) c0
  r1 <- evalCompOrFail (pick c1) c1
  r2 <- evalCompOrFail (pick c2) c2
  let result = r0 + r1 + r2 + dWord + fromIntegral i
  when isTop $
    compSinkReq docSinkId (HashMapStoreReq (docKey i) (BSC.pack (show result)))
  pure result

-- | Wires all 'levelsCount' levels (each with 'defsPerLevel' defs), bottom
-- to top, threading each level's defs and size down as "prev" for the next.
buildLevels :: [Word32] -> CompWireM [Level]
buildLevels sizes = go (0 :: Int) Nothing sizes
 where
  numLevels = length sizes
  go :: Int -> Maybe (Level, Word32) -> [Word32] -> CompWireM [Level]
  go _ _ [] = pure []
  go lvl mPrev (sz : rest) = do
    let isTop = lvl == numLevels - 1
    curLevel <- mapM (wireDef lvl isTop mPrev) [0 .. defsPerLevel - 1]
    restLevels <- go (lvl + 1) (Just (curLevel, sz)) rest
    pure (curLevel : restLevels)
  wireDef :: Int -> Bool -> Maybe (Level, Word32) -> Int -> CompWireM (Comp Word32 Word64)
  wireDef lvl isTop mPrev d =
    wireComp $ defineComp name fullCaching bodyFun
   where
    name = "l" ++ show lvl ++ "_d" ++ show d
    dWord = fromIntegral d :: Word64
    bodyFun = case mPrev of
      Nothing -> level0Body dWord
      Just (prevLevel, prevSize) -> higherLevelBody prevLevel prevSize isTop dWord

-- | Wires the full graph plus its root: for @i@ in @[0, topSize)@, grouped
-- by @i \`mod\` defsPerLevel@, evaluate every top-level def at its group's
-- params (sequential 'mapM' over 'evalCompOrFail', mirroring the Rust
-- benchmark's use of @eval_all@).
wireAllComps :: [Word32] -> CompWireM (Comp () ())
wireAllComps sizes = do
  levels <- buildLevels sizes
  let topLevel = last levels
      topSize = last sizes
      groups = [[i | i <- [0 .. topSize - 1], i `mod` fromIntegral defsPerLevel == g] | g <- [0 .. fromIntegral defsPerLevel - 1]]
  wireComp $
    defineComp "root" fullCaching $ \() -> do
      let !_ = bumpRerunCounter ()
      forM_ (zip topLevel groups) $ \(comp, is) -> mapM_ (evalCompOrFail comp) is

----------------------------------------
-- Memory measurement
----------------------------------------

-- | Shells out to @ps -o rss= -p \<this process\>@ (same method as the Rust
-- benchmark's @rss_mb@) and returns resident set size in MB (0 if @ps@'s
-- output doesn't parse).
getRssMb :: IO Double
getRssMb = do
  pid <- getProcessID
  out <- readProcess "ps" ["-o", "rss=", "-p", show pid] ""
  pure $ case words out of
    (kbStr : _) -> maybe 0 (\kb -> fromIntegral (kb :: Int) / 1024) (readMaybe kbStr)
    [] -> 0

----------------------------------------
-- Orchestration
----------------------------------------

waitForRunAtLeast :: TVar (Option RunStats) -> Int -> IO RunStats
waitForRunAtLeast runVar n = atomically $ do
  mrs <- readTVar runVar
  case mrs of
    Some rs | rs_run rs >= n -> pure rs
    _ -> retry

benchMain :: IO ()
benchMain = do
  scale <- readScale
  let sizes = scaledLevelSizes scale
      targetInstances = 1000000 * scale :: Double
      topSize = last sizes

  putStrLn "=== bench: graph shape (persistence-free port of Rust persist_bench) ==="
  printf "PERSIST_BENCH_SCALE=%.4f (target ~%.0f achieved instances)\n" scale targetInstances
  printf "levels: %d, defs/level: %d, total defs: %d\n" levelsCount defsPerLevel (levelsCount * defsPerLevel)
  putStrLn ("declared level sizes (bottom -> top): " ++ show sizes)
  printf "source keys: %d, top-level sink outputs: %d\n" srcKeys topSize
  putStrLn ""

  kv <- initHashMapFlow "bench-kv"
  docSink <- initHashMapFlow "bench-doc-sink"

  -- Pre-populate the 300 default source keys (k -> k*7+3), mirroring the
  -- Rust benchmark's make_kv.
  forM_ [0 .. srcKeys - 1] $ \k ->
    hmfInsert kv (BSC.pack (show k)) (BSC.pack (show (fromIntegral k * 7 + 3 :: Word64)))

  runVar <- newTVarIO None
  t0 <- getCurrentTime
  engineHandle <- async (compDriver' runVar (withCompFlows kv docSink) (wireAllComps sizes) ())

  -- Cold settle: the *entire* initial evaluation runs synchronously inside
  -- Impl.startCompEngine, before the driver loop's first
  -- rcif_shouldStartWithRun call is even made -- so the first time
  -- compDriver' writes to runVar (run == 1) is exactly the moment the cold
  -- eval has finished.
  rs1 <- waitForRunAtLeast runVar 1
  tCold <- getCurrentTime
  coldReruns <- readRerunCounter
  rssCold <- getRssMb

  let coldWallTime = realToFrac (diffUTCTime tCold t0) :: Double
  putStrLn "--- 1. cold eval (no persistence) ---"
  printf
    "achieved instance count: %d (target ~%.0f, %+.1f%%)\n"
    coldReruns
    targetInstances
    (100 * (fromIntegral coldReruns - targetInstances) / targetInstances)
  printf "wall time: %.3f s\n" coldWallTime
  printf "RSS after cold settle: %.1f MB\n" rssCold

  -- Assert every top-level doc was written, mirroring the Rust benchmark's
  -- own assertion.
  docsAfterCold <- getHashMap docSink
  let expectedDocs = fromIntegral topSize :: Int
  when (HashMap.size docsAfterCold /= expectedDocs) $
    ioError
      ( userError
          ( "expected "
              ++ show expectedDocs
              ++ " top-level docs written, got "
              ++ show (HashMap.size docsAfterCold)
          )
      )

  -- The 300 pre-population inserts above are still sitting as unconsumed
  -- "changes" on the kv source (see HashMapFlow's changesVar): the driver
  -- loop's first real iteration drains them. Everything that read those
  -- keys during the cold eval recorded exactly the version now being
  -- reported as "changed", so this drains as a strictly version-matching
  -- no-op (0 reruns expected) -- but we still wait for it to fully
  -- complete before mutating, so the live-update measurement below is
  -- attributable to *only* our mutation.
  rs2 <- waitForRunAtLeast runVar (rs_run rs1 + 1)
  preLiveReruns <- readRerunCounter
  when (preLiveReruns /= coldReruns) $
    putStrLn
      ( "NOTE: draining leftover pre-population changes triggered "
          ++ show (preLiveReruns - coldReruns)
          ++ " unexpected reruns (expected 0)"
      )

  -- 2. Live incremental update: mutate exactly one source key on the
  -- still-running engine and time until the driver's next propagation
  -- round completes.
  tBeforeMutate <- getCurrentTime
  hmfInsert kv "0" "13371337"
  _rs3 <- waitForRunAtLeast runVar (rs_run rs2 + 1)
  tAfterMutate <- getCurrentTime
  liveReruns <- readRerunCounter
  let liveWallTime = realToFrac (diffUTCTime tAfterMutate tBeforeMutate) :: Double
      liveRerunCount = liveReruns - preLiveReruns

  putStrLn ""
  putStrLn "--- 2. live incremental, 1 changed input (no persistence) ---"
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

  when (coldReruns > 0) $ do
    printf
      "bytes/instance (RSS after cold settle / instances): %.1f B/instance\n"
      (rssCold * 1000000 / fromIntegral coldReruns :: Double)
    printf
      "bytes/instance (GHC max_live_bytes / instances): %.1f B/instance\n"
      (fromIntegral (max_live_bytes rtsStats) / fromIntegral coldReruns :: Double)

  cancel engineHandle
