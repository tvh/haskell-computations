# The reference engine at 1M caps — benchmark notes (Haskell)

> Working notes, not polished prose. Companion to
> `rust-computations/docs/persistence-benchmark-notes.md`, which benchmarks the
> Rust port of this engine through five optimization stages and *estimated* this
> reference implementation's numbers on paper (its "Interlude" sections). This
> doc holds the real, measured numbers. Same machine: Apple-silicon Mac
> (Darwin 25.5). Reproduce with:
> `stack exec -- incremental-computations-exe bench +RTS -T -N`
> (`PERSIST_BENCH_SCALE=0.1` for the ~100k variant; the benchmark self-reports
> wall times, rerun counts, RSS, and GHC RTS stats.)

## System under test

- This repository as-is: the reference implementation of the coarse-grained
  self-adjusting computation engine from Wehr's FUNARCH '23 paper. `fullCaching`
  cache behavior, `SimpleStateIf` state (five persistent containers in a
  `TVar`), stock driver loop. **No persistence exists and none is simulated** —
  so the comparable Rust figures are its *engine-only* numbers (its phases 5
  and 7), never the with-persistence ones.
- Benchmark graph: identical to the Rust `persist_bench` topology, formula for
  formula — 50 definitions × 10 levels, fixed fan-in 3 via the same
  modular-arithmetic edge formulas over `Word32` params, 300 in-memory KV
  inputs (`HashMapFlow` as source), 1000 top-level sink outputs (a second
  `HashMapFlow` as sink), front-loaded level sizes
  `[205000 ×5, 128945, 101885, 61090, 27060, 1000]` → **999,760 instances**
  achieved at scale 1.0 (bit-identical to the Rust achieved count — the
  strongest available check that the two benchmarks build the same graph).
  Bodies are trivial `Word64` additions — deliberately, so the numbers measure
  the *engine overhead floor*, not body cost.
- Benchmark code: `app/Control/Computations/Demos/Bench/Main.hs` (`bench`
  subcommand of the demo executable). Two phases, one process:
  1. **cold eval** — engine start to initial evaluation settled;
  2. **live incremental** — mutate exactly one source key (`"0"` →
     `"13371337"`) on the still-running engine, time until the propagation
     round *fully* completes.

## Measurement methodology (differs from Rust in two places worth knowing)

- **Rerun counting is done at the engine's interface boundary, not inside
  bodies.** `CompM` has no `liftIO`, and an `unsafePerformIO` counter inside a
  "pure" body is unsound (GHC may float/share/eliminate it — the first version
  of this benchmark hit exactly that). Instead `countingStateIf` wraps the
  `CompEngineStateIf` record so `capEvaluationStarted` — which `Impl` calls
  exactly once immediately before each real body invocation — bumps an
  `IORef` before delegating. Same event the Rust benchmark's
  `run_counter.fetch_add` observes, observed from outside.
- **Settle detection must survive time-sliced rounds.** The driver processes at
  most `rcif_maxLoopRunTime` (10 s) of stale caps per loop iteration, so at
  scale 1.0 a single mutation's ~80k-rerun cascade spans multiple `RunStats`
  "runs", each carrying leftover `rs_staleCaps` into the next. A naive
  wait-for-next-run reported 10.0 s / 34,356 reruns — a partial round.
  `waitForFullSettle` polls run by run until a run reports an *incoming*
  backlog of zero. (The Rust benchmark's per-round tracing signal has no
  equivalent slicing, so it never needed this.)
- RSS via `ps -o rss=` (same as Rust's `rss_mb`); live heap via `GHC.Stats`
  (`+RTS -T`): `max_live_bytes` is the GC-measured peak live set,
  `max_mem_in_use_bytes` what the RTS actually took from the OS.

## Stage 0 — the reference engine, unmodified (commits `7da35b2`, `46f68b7`, `5e4ab4c`)

| Scale | Instances | Cold eval | RSS after settle | Live reruns | Live update |
|---|---|---|---|---|---|
| 0.02 | 20,254 | 0.48–0.50 s | 149 MB | 1,677 | 47–50 ms |
| 0.1 | 98,982 | 2.56–2.85 s | 448 MB | 8,248 | 0.41–0.45 s |
| 1.0 (run A) | **999,760** | 42.80 s | 2,571 MB | **80,767** | 24.40 s |
| 1.0 (run B) | **999,760** | 42.98 s | 1,783 MB | **80,767** | 31.44 s |

At 1.0, both runs: `max_live_bytes` **1,501.3 MB** (bit-identical across runs),
`max_mem_in_use_bytes` ~3,592 MB, 18,516 GCs. Per-instance: **~1,502 B/cap
live** (`max_live_bytes` basis, the stable figure), ~1,783–2,571 B/cap on the
noisier RSS basis. Per-node timing: cold ~43 µs/instance; live ~300–390
µs/rerun.

### Findings

- **Interlude 1's paper estimate holds up.** The Rust notes statically tallied
  this engine's data types at ~1,350 B/cap (±25%) → ~1.35 GB live at 1M.
  Measured: 1,501 B/cap — **+11%, well inside the stated uncertainty**. Its RSS
  prediction ("~2.7–4 GB for a 1.35 GB live set") matches
  `max_mem_in_use_bytes` (~3.6 GB); `ps` RSS came in below that (1.8–2.6 GB),
  presumably address space the RTS holds but the OS hasn't resident-charged.
- **Topology fidelity is exact.** 999,760 cold instances and 80,767
  live-update reruns are the *same numbers* the Rust benchmark reports at this
  scale — the modular fan-in formulas, `Word32` wrapping and all, reproduce the
  identical graph and identical dirty set.
- **Where this lands vs. the Rust port** (its Stage 5 / Tier 2 engine-only
  figures, same graph):
  - live heap ~4.6× (1,502 vs ~330 B/node);
  - cold eval ~14× (43 s vs ~2.9–3.0 s);
  - live incremental ~50–60× (24–31 s vs ~0.5 s).
  The memory ratio is exactly the story the Rust notes' interludes told; the
  *time* ratios are new information — the interludes never estimated timing.
  The live-update gap (µs/rerun: ~300–390 vs ~6) is the most dramatic and the
  least explained; candidate suspects (unmeasured): `Data.Map`/HAMT traversal
  constants across the five containers per touched cap, `Ord AnyCompAp`'s
  `eqT`-per-comparison, `show`-rendering of every recomputed result into
  `ccm_logrepr` (`fullCaching`), and GC pressure from churn allocation
  (`mkCompAp` allocates fresh `CompAp`/`AnyCompAp` per parent eval call).
- **Run-to-run**: rerun counts and `max_live_bytes` are deterministic
  (bit-identical). Wall times varied <0.5% cold but ~29% on the live update,
  and RSS-after-settle ~30% — stop-the-world-GC-sensitive measurements on
  shared laptop hardware. More trials would tighten this.

## Caveats

- Bodies are trivial additions: this is the engine floor, same caveat as the
  Rust doc. Real workloads shift every comparison toward "body cost dominates".
- ~8% of the graph re-running for 1 changed input is the fan-in-3 topology,
  not overhead.
- Two runs at 1.0 is thin for the noisy measurements (live wall time, RSS).
- Single process, cold-then-live: RSS-after-settle is measured *after* the
  cold eval in the same process the live update then reuses — comparable to
  Rust's phase-7 setup, but not to its isolated process-per-phase RSS figures
  except phase 7's own.

## Runtime research — where the time goes (reasoning + one probe, before touching code)

Question: can the 14× cold / 50–60× live-update gap vs. the Rust port be
closed without abandoning the architecture? Findings, in order of confidence:

### Build/RTS configuration was leaving easy money on the table

- The build is stack's default `-O1`; `-O2` is set nowhere.
- The executable hardcodes `-with-rtsopts=-N`: every benchmark number above
  ran with all-cores parallel GC and the default 4 MB nursery — hence the
  18,516 GCs at scale 1.0. Probed at scale 0.1 (same build, `5e4ab4c`):

| RTS flags | cold eval | live update | GCs | RSS after settle |
|---|---|---|---|---|
| default (`-A4m -N`) | 2.48 s | 0.367 s | 1,043 | 447 MB |
| `-A16m` | 2.14 s | 0.359 s | 24 | 560 MB |
| `-A64m` | **1.65 s** | **0.306 s** | **4** | 1,122 MB |
| `-N1` | — | — | — | crashes |

  **−33% cold / −17% live from a nursery flag alone**, at a real memory
  price (nurseries are per-capability under `-N`; `-n` chunking or a small
  fixed `-N` would moderate it). The `-N1` crash is a genuine finding:
  "thread blocked indefinitely in an STM transaction" right after cold
  settle — the driver deadlocks on a single capability.

### The monad is not naively slow — the classic fixes are already in

`CompM` is a Haxl-shaped resumption monad: `CompFinished`/`CompSuspended`
mirror Haxl's `Done`/`Blocked` ("There is no Fork", ICFP'14), including the
applicative `CompReqCombined` batching; and `ContCompM` (`Types.hs`) already
implements the Jaskelioff/Rivas "smart view" — the same fix family as
"Reflection without Remorse" and the FTCQueue in "Freer Monads, More
Extensible Effects". The quadratic left-bind pathology that literature
addresses is already solved here; switching encodings (freer, Church/
Codensity à la "Free Monads for Less") would shave constants, not close a
50× gap.

### What actually looks structurally slow (unmeasured — profile first)

1. **A `HashSet` union per bind.** `compMBind`/`compMAp` thread the `DepSet`
   as a writer monoid (`w <> w'`, `Types.hs`) — every bind in every rerun
   allocates and unions. Haxl's lesson applies: `GenHaxl` is a monad *over
   IO* with mutable state; deps could accumulate into a per-cap mutable ref
   (the state-if is already `IO`), deleting per-bind unions outright.
2. **Every child eval is a suspend→engine→resume round trip, even on a
   cache hit**, and the cache lookup is `Data.Map` keyed by `AnyCompAp`
   whose `Ord` pays an `eqT` fingerprint comparison per step — ~3M edges ×
   ~20 comparisons at scale 1.0. Interning (Interlude 2's big item) is a
   *time* win too, not just memory.
3. **MD5 per `mkCompAp`** — every parent eval call MD5-hashes
   `(name, param)` via large-hashable; ~3M+ hashes on a cold eval. A
   non-crypto 128-bit hash (e.g. xxh3-128) is ~10× faster on small inputs;
   the Rust port uses truncated blake3.
4. **`fullCaching` `show`s and MD5s every recomputed result**
   (`CacheBehaviors.hs`) — the `ccm_logrepr` memory item is also a
   per-rerun time cost.

### If a restructure is ever warranted

The modern consensus (effectful/bluefin benchmarks) is that free-monad-family
interpreters lose 1–3 orders of magnitude to evidence-passing/IO-based
designs. The relevant move here is not a better free monad but **CompM over
IO with real suspension only on cache miss**: GHC ≥ 9.6 ships delimited
continuation primops (`prompt#`/`control0#`, GHC proposal 313 — the
machinery behind Alexis King's eff), and this repo is on GHC 9.10.3. Cache
hits become direct mutable-map reads; a continuation is captured only when
the engine genuinely needs to intervene. Biggest lever, deepest cut — gate
it on profiling evidence.

### Recommended order

1. Profile (`-fprof-late`) to split time across suspects 1–4.
2. `-O2` + RTS tuning (free, ~30%+ demonstrated).
3. MD5 → xxh3; drop the per-rerun `show`.
4. Interning + dense lookup (shared with the memory diet).
5. Only then judge the IO-based `CompM` rewrite.

## Not tried yet / open items

- **The Interlude-2 diet, measured.** The Rust notes list a ranked, concrete
  diet for this codebase (delete `sifs_vermap`; move `ccm_logrepr`/
  `ccm_approxCachedSize` to per-def `CompCacheBehavior`; drop the
  `OM.insert key mempty` for output-less caps; then interning + flattening the
  cache-value box tower) with a projected ~1,350 → ~565–720 B/cap. This
  benchmark is the before/after instrument for that work.
- RTS-flag sensitivity: `-F` (old-gen factor), `--nonmoving-gc`, `-c`
  (compacting), heap-size hints — the copying-GC multiplier is a large share
  of RSS and entirely untested.
- `+RTS -s` max residency vs. `max_live_bytes` cross-check; a heap profile
  (`-hT`) to attribute the 1.5 GB live set by closure type against
  Interlude 1's per-container tally.
- More 1.0 trials for variance on the live-update time.
- Timing attribution for the ~300–390 µs/rerun live-update cost (see
  suspects above).
- The persistence port itself (Interlude 3 sketches it: LMDB store, revival
  from `(Comp, typed param)`, STM-based debounced flush) — out of scope until
  the engine-only numbers are understood.
