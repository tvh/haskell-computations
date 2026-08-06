# A framework for coarse-grained self-adjusting computations in Haskell

Keeping what a user sees in sync with data that keeps changing is work you
normally do by hand. Self-adjusting computations do it for you: when an
input changes, only the outputs that actually depend on it get recomputed.

This is the coarse-grained variant. Dependencies are tracked per
computation rather than per data-flow edge inside one, which keeps the
bookkeeping cheap enough to run over a million of them.

The framework was extracted from a commercial product built by medilyse
GmbH, Freiburg, Germany. I maintain this fork, with a rewritten engine core
and a narrowed public API.

The repository also contains three demo applications.

There is an accompanying article:

> Stefan Wehr. 2023. A Software Architecture Based on Coarse-Grained
> Self-Adjusting Computations. Proceedings of FUNARCH 2023.
> Seattle, WA, USA. ACM, 2023.
> [Preprint](Wehr_A-Software-Architecture-Based-on-Coarse-Grained-Self-Adjusting-Computations.pdf)

The paper describes the architecture in depth.

## What this library gives you

A *computation* (`Comp`/`CompDef`) is a named, typed step. It takes an input
and produces an output, optionally by requesting data from other
computations or from external *sources*, and optionally by writing results
to external *sinks*.

You wire computations into a dependency graph and drive it with a
`compDriver`. When a source changes, the engine works out which computations
depend on that change and reruns only those.

Early cutoff falls out of this: if a rerun produces the same result as
before, everything downstream keeps its cached value and never runs at all.

Sources and sinks are pluggable. The library ships six flow implementation
modules:

| Module | What it is |
|---|---|
| `FileSrc` / `FileSink` | read from and write to the filesystem, with output tracking so files a computation no longer produces get cleaned up |
| `HashMapFlow` | an in-memory key/value source *and* sink. Reach for this when testing your own computations, or trying the API without touching a filesystem |
| `TimeSrc` | periodic ticks, for computations that should rerun as the clock passes a threshold |
| `IOSink` | an escape hatch for running plain `IO` from a computation body |
| `CompLogging` | structured logging from inside a computation body, built on `IOSink` |

NOTE: `IOSink` is the only route from a computation body to `IO`, and what
it costs you is real -- effects run that way are invisible to dependency
tracking and to output cleanup. Its haddock spells out when that is fine and
when it isn't.

You can also write your own by instantiating the `CompSrc`/`CompSink`
classes. `HashMapFlow` is a compact example of a type that does both.
`FileSink` is the one to read for getting output tracking right.

## Installing

Not on Hackage yet. Nothing blocks an upload -- every dependency resolves
from a released Stackage snapshot -- it just hasn't happened. To depend on
it from another `stack` project in the meantime, add it as an `extra-dep`
pointing at this repository in your `stack.yaml`, e.g.:

```yaml
extra-deps:
  - github: tvh/haskell-computations
    commit: <commit-sha>
```

and add `incremental-computations` to your package's `dependencies:` in
`package.yaml` (or `build-depends:` in a hand-written `.cabal` file).

## Building this repository

Requirements: the [`stack`](https://docs.haskellstack.org/en/stable/) build
tool. The repository pins its GHC and package set via `stack.yaml`
(`lts-24.51`, GHC 9.10.3), so any reasonably recent `stack` will do.

Build:

```
$ stack build
```

There are two test suites. `stack test` runs the library's; `stack run --
test` runs the demos':

```
$ stack test
$ stack run -- test
```

## Development

`-Werror` is **not** part of the default build. It sits behind a cabal flag
(`werror`, off by default) so a warning added by a future GHC release can't
break a downstream build. The `Makefile` turns it on (`make all`,
`make test`), so warnings still fail locally. To turn it on directly with
`stack`:

```
$ stack build --flag incremental-computations:werror
$ stack test  --flag incremental-computations:werror
```

## A minimal worked example

Everything below compiles against the public API alone -- nothing here
reaches past `Control.Computations.CompEngine` and the shipped `FlowImpls`.
It defines three computations: one counts the lines in a file, one sums a
list of such counts, one writes the total out. They are wired to a
`FileSrc`/`FileSink` pair plus the built-in `IOSink` for logging.

Adapted from
[`app/Control/Computations/Demos/Simple/Main.hs`](app/Control/Computations/Demos/Simple/Main.hs)
-- the module header is stripped and the comments are written for reading
rather than for haddock.

```haskell
import Control.Computations.CompEngine
import Control.Computations.FlowImpls.FileSink
import Control.Computations.FlowImpls.FileSrc
import Control.Computations.FlowImpls.IOSink
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging

import Prelude hiding (readFile, writeFile)

-- A computation from a file path to the number of lines in that file. Takes
-- the FileSrc's typed id as an argument rather than referencing a top-level
-- id, so the id's instance name is only ever written down once (see `main`
-- below).
numberOfLinesCompDef :: TypedCompSrcId FileSrc -> CompDef FilePath Int
numberOfLinesCompDef fileSrcId =
  defineComp "numberOfLines" fullCaching $ \p -> do
    string <- compSrcReq fileSrcId (ReadTextFile p)
    pure (length (lines string))

-- A computation that reads a file of paths (one per line) and sums the
-- line counts of each, by calling back into another computation.
sumCompDef
  :: TypedCompSrcId FileSrc
  -> Comp FilePath Int
  -> CompDef FilePath Int
sumCompDef fileSrcId c = defineComp "sum" fullCaching $ \p -> do
  string <- compSrcReq fileSrcId (ReadTextFile p)
  list <- mapM (evalCompOrFail c) (lines string)
  pure (sum list)

-- The top-level computation: run the sum over a fixed file and write the
-- result out through the sink.
storeCompDef :: TypedCompSinkId FileSink -> Comp FilePath Int -> CompDef () ()
storeCompDef fileSinkId c = do
  defineComp "store" fullCaching $ \() -> do
    i <- evalCompOrFail c "file_list.txt"
    compSinkReq fileSinkId (WriteTextFile "output.txt" ("number of lines: " ++ show i))

-- Wiring: turn each CompDef into a runnable Comp, chaining them together,
-- against the actual source/sink ids passed in by `main`.
wireAllComps :: TypedCompSrcId FileSrc -> TypedCompSinkId FileSink -> CompWireM (Comp () ())
wireAllComps fileSrcId fileSinkId = do
  numberOfLinesC <- wireComp (numberOfLinesCompDef fileSrcId)
  sumC <- wireComp (sumCompDef fileSrcId numberOfLinesC)
  wireComp (storeCompDef fileSinkId sumC)

-- Register the already-built flow instances, then hand control to the
-- given action.
withCompFlows :: FileSrc -> FileSink -> CompFlowRegistry -> IO () -> IO ()
withCompFlows fileSrc fileSink reg action = do
  registerCompSrc reg fileSrc
  registerCompSink reg fileSink
  registerCompSink reg ioSink
  action

-- Build the instances once, up front, then drive the graph until it
-- settles. Both the ids and the registration derive from these same
-- instances, so each name is written down exactly once.
main :: IO ()
main = withSysTempDir $ \tgt ->
  withFileSrc (defaultFileSrcConfig "fileSrc") $ \fileSrc -> do
    fileSink <- makeFileSink "fileSink" tgt
    logNote ("Target directory: " ++ tgt)
    compDriver
      (withCompFlows fileSrc fileSink)
      (wireAllComps (typedCompSrcIdOf fileSrc) (typedCompSinkIdOf fileSink))
      ()
```

Run it as the `simple` subcommand of the demo executable:

```
$ stack run -- --log-level info simple
```

## Demo: Directory sync

This demo synchronizes the content of one directory to another. The
sync works recursively and continuously.

[Source code](app/Control/Computations/Demos/DirSync)

To sync directory `A` to directory `B`: `A` must exist, `B` is created
automatically.

NOTE: everything in `B` that does not exist in `A` will be deleted.

```
$ stack run -- --log-level info sync --source A --target B
```

## Demo: Hospital

A processing pipeline for hospital data. It is deliberately basic: patients
are admitted and discharged, and notes can be added to a patient. A
simulation creates the stream of events that drives it.

[Source code](app/Control/Computations/Demos/Hospital)

Start the simulation:

```
$ stack run -- --log-level info hospital-simulation --root run-data
```

Start the pipeline:

```
$ stack run -- --log-level info hospital-pipeline --config hospital-config/ --root run-data
```

Now output files should appear in `run-data/output`.

You can also change data by hand. The sqlite database for patients is in
`run-data/pats.sqlite`, the database for patient notes in
`run-data/pat_notes.sqlite`.

Start the webserver:

```
stack run -- --log-level info hospital-server --out run-data/output --web webapp
```

Then point your browser to http://localhost:8080

## Benchmark

The scale benchmark (`incremental-computations-bench`) lives in its own
cabal `benchmarks:` stanza, so its dependencies don't leak into the library
or the test suite. Run it with:

```
$ stack bench
```

`PERSIST_BENCH_SCALE` scales the synthetic dependency graph. It defaults to
`1.0`, a ~1,000,000-instance run. For a quicker ~100,000-instance run:

```
$ PERSIST_BENCH_SCALE=0.1 stack bench
```

A second benchmark, the Hospital pipeline benchmark, exercises concurrent
source dispatch (see below) with a graph whose comp bodies build real
applicative batches and whose source has configurable latency to hide —
things the scale benchmark's graph structurally cannot do. Run it with:

```
$ HOSPITAL_BENCH=1 stack bench
```

Env vars: `HOSPITAL_BENCH_SCALE` (default `1.0`, ~1,000 patients across 20
wards, ~976,000 instances) scales the patient/ward count continuously;
`HOSPITAL_BENCH_SRC_LATENCY_US` (default `0`) adds a simulated per-call
source latency, in microseconds, to stand in for a real service call. This
benchmark has no concurrency knob of its own (see `docs/benchmark-notes.md`'s
Stage 12/12a for why).

After its cold eval and single-key live update, the Hospital benchmark also
runs a rerun-heavy live phase: it mutates many source keys in one batch
(spread across patients and all five sources) and reports keys mutated,
wall time, reruns, and µs/rerun -- the number future rerun-path work should
be judged against. `HOSPITAL_BENCH_RERUN_KEYS` (default `400`, ~3,069
reruns at scale 1.0; `0` disables the phase) sets keys mutated per round;
`HOSPITAL_BENCH_RERUN_LOOPS` (default `1`) repeats the round, mirroring the
scale benchmark's `PERSIST_BENCH_LIVE_LOOPS` diagnostic (see
`bench/Control/Computations/Demos/Bench/Main.hs`).

A third benchmark, the tiered pipeline benchmark, extends Hospital's graph
with per-source latency tiers (five sources, a ~40x spread instead of one
shared latency) and four comps that read a source alongside their existing
comp dependencies in the same applicative batch — the mixed source/comp
shape needed to actually exercise concurrent computation evaluation (see
below), which Hospital's own graph only mixes in a single comp. Run it
with:

```
$ TIERED_BENCH=1 stack bench
```

Env vars: `TIERED_BENCH_SCALE` (default `0.18`, ~180 patients across ~4
wards) scales the patient/ward count continuously; `TIERED_BENCH_LATENCY_MULT`
(default `1.0`) scales the five per-source latency tiers (25 μs to 1 ms
base latency), `0` disables simulated latency for a cheap structural-only
run; `TIERED_BENCH_JITTER` (default off) adds deterministic per-key jitter
to each simulated round trip; `TIERED_BENCH_BUNDLING` (default on) toggles
whether same-instance multi-key requests bundle into one round trip;
`TIERED_BENCH_EVAL_CONCURRENCY` (default `1`) sets the eval concurrency
width (`setCompEvalConcurrency`, see below) — which also governs how much
source dispatch (see below) can overlap, since the two now share one
permit pool. Like Hospital, it also runs a rerun-heavy live phase, controlled
the same way: `TIERED_BENCH_RERUN_KEYS` (default `400`, `0` disables the
phase) and `TIERED_BENCH_RERUN_LOOPS` (default `1`).

[`docs/benchmark-notes.md`](docs/benchmark-notes.md) has measured numbers
from prior runs, stage by stage, with what was kept and what was reverted.

NOTE: those are working notes, not polished docs.

## Concurrent flow execution

By default, every request a comp body makes — even ones batched together
via `<*>` into one `CompReqCombined` — runs one at a time on the engine
thread. A `CompSrc` declares whether it tolerates concurrent calls to its
own `compSrcExecute` via `compSrcConcurrency`, a plain two-state value:
`FlowSerial` (the default) or `FlowConcurrent`. A `CompSink` declares the
same thing for its own `compSinkExecute` (`compSinkConcurrency`, also
defaulting to `FlowSerial`) — see "Concurrent computation evaluation" below
for why a sink needs an opinion here at all once cap evaluation itself can
run on more than one thread.

There is no separate width knob for source (or sink) dispatch — an earlier
one existed and was removed as dead weight, measured flat on two
independent realistic graph shapes (see `docs/benchmark-notes.md`'s Stage
12/12a). A `FlowConcurrent` source's leaves may instead be proactively
dispatched against the *eval* concurrency pool ("Concurrent computation
evaluation", below): dispatch draws permits from that same shared pool
rather than a separately-sized one, so how much source dispatch can
actually overlap is governed entirely by `setCompEvalConcurrency`. A
`FlowSerial` instance, or any instance reached while parallel eval is off,
is never proactively dispatched — it always runs inline, in leaf order, on
the engine thread, exactly as before this mechanism existed. Losing the
race for a permit is not a failure, either: an undispatched `FlowConcurrent`
group simply runs at its own leaf's position instead, so dispatch is purely
a performance optimization, never a correctness requirement.

`HashMapFlow`, `TimeSrc`, and `FileSrc` all declare `FlowConcurrent`. A
source that hasn't opted in stays serialized no matter how much eval
concurrency is available. See `docs/benchmark-notes.md`'s Stage 5 for the
original design and measured numbers.

## Concurrent computation evaluation

By default, the eval leaves of an applicative batch — the nested cap
evaluations inside a `CompReqCombined`, as distinct from the source leaves
the section above governs — also run one at a time, on the engine thread.
`setCompEvalConcurrency` lets eval leaves fork to a permit-bounded pool
instead. A promise table, one IVar per in-flight cap, guarantees each cap
is still evaluated at most once per occasion no matter how many leaves
reference it or how wide the pool is.

To turn it on, call `setCompEvalConcurrency` inside the same
`withRegisteredFlows` callback:

```haskell
compDriver
  (\reg action -> do
      setCompEvalConcurrency reg (mkCompEvalConcurrency 64)
      registerCompSrc reg mySrc
      action)
  wireComps
  ()
```

The default width is 1: no fork pool is allocated at all, and every eval
leaf runs exactly as it did before this knob existed, and no `FlowConcurrent`
source or sink ever gets proactively dispatched either (see "Concurrent flow
execution", above). Nothing changes unless you opt in.

This is the *only* concurrency width knob the engine exposes: source
dispatch draws from this same pool rather than a separately-sized one of
its own (an earlier version of this project had a second, source-side width
knob; it was removed as dead weight — see `docs/benchmark-notes.md`'s Stage
12/12a). It must be set before the engine starts: it is read exactly once,
at engine start, so a call after that has no effect on an already-running
engine.

**Measured effect.** On the tiered benchmark (see Benchmark, above),
`docs/benchmark-notes.md`'s Stage 15 measured, within one session, cold
eval going from 71.9 s at width 1 to 4.3 s at width 64 — 16.6x — against
56.4 measured source-seconds of latency to hide, i.e. 13.1x effective
concurrency against ~2.5 s of genuinely serial engine work. The ceiling
sits around width 64 and is close to the floor: widths 128 and 256 measure
the same 4.4 s. NOTE: only within-session ratios are meaningful here — this
benchmark has documented run-to-run drift (`docs/benchmark-notes.md`'s
Stage 12a), so don't compare this section's absolute seconds against a
different run's. See Stages 13-15 for the full investigation, including
the fan-out/permit-pool mismatch this fixes and the permit-release bug
found and fixed along the way.

### What ordering you still get

Still guaranteed at any eval width:

- dependency correctness — a cap's own dependency set is only ever written
  by the thread evaluating that cap;
- dedup, now complete at any width — every reference to the same cap within
  one occasion sees one evaluation and one result, `hashCaching` included;
- left-error bias, and the leftmost-failing-leaf guarantee;
- every fork is joined before its batch returns, including on the
  exception path;
- width 1 is byte-identical to the engine as it existed before this knob,
  evaluation order included.

No longer guaranteed above width 1:

- the interleaving of `capEvaluationStarted` across a batch's leaves;
- the relative order of sink writes issued by *different* caps (within one
  cap's own body, order is still preserved — this is also why
  `compSinkConcurrency` exists, see above).

Read this before turning the width up in production — it's the part most
likely to surprise you. See `docs/benchmark-notes.md`'s Stage 15, "The
ordering contract, which changed", for the full reasoning.
