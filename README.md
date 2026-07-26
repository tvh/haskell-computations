# A framework for coarse-grained self-adjusting computations in Haskell

Ensuring that software applications
presents their users the most recent version of data is not trivial.
Self-adjusting computations are a technique for automatically and efficiently recomputing
output data whenever some input changes.

This repository contains the code of a framework for coarse-grained self-adjusting computations in Haskell. The framework has been extracted from
a commercial software product developed by medilyse GmbH, Freiburg, Germany, and
is maintained here as a fork with a rewritten engine core and a narrowed,
published API.

The repository also contains two demo applications using the framework.

There is an accompanying article:

> Stefan Wehr. 2023. A Software Architecture Based on Coarse-Grained
> Self-Adjusting Computations. Proceedings of FUNARCH 2023.
> Seattle, WA, USA. ACM, 2023.
> [Preprint](Wehr_A-Software-Architecture-Based-on-Coarse-Grained-Self-Adjusting-Computations.pdf)

The paper describes the architecture in depth; the sections below give a
practical, code-first introduction to the same ideas.

## What this library gives you

A *computation* (`Comp`/`CompDef`) is a named, typed step: given an input, it
produces an output, optionally by requesting data from other computations or
from external *sources*, and optionally writing results to external *sinks*.
Computations are wired together into a dependency graph and driven forward by
a `compDriver`. When an underlying source changes, the engine works out which
computations actually depend on that change, reruns only those, and leaves
everything else's cached result alone — including anything downstream that
turns out, after rerunning, to still produce the same output. This is what
"coarse-grained self-adjusting" means: correctness comes from tracking
dependencies at the level of whole computations (not fine-grained data-flow
edges inside them), while still avoiding wasted recomputation.

Sources and sinks are pluggable. The library ships six flow implementations:

| Module | What it is |
|---|---|
| `FileSrc` / `FileSink` | read from and write to the filesystem, with output tracking so files a computation no longer produces get cleaned up |
| `HashMapFlow` | an in-memory key/value source *and* sink — the one to reach for when testing your own computations, or trying the API without touching a filesystem |
| `TimeSrc` | periodic ticks, for computations that should rerun as the clock passes a threshold |
| `IOSink` | an escape hatch for running plain `IO` from a computation body (see its haddock for what "escape hatch" costs you) |
| `CompLogging` | structured logging from inside a computation body, built on `IOSink` |

You can also implement your own by instantiating the `CompSrc`/`CompSink`
classes; `HashMapFlow` is a compact worked example of a type that does both,
and `FileSink` is the reference for getting output tracking right.

## Installing

This package has not been uploaded to Hackage yet, though nothing blocks it
from being — every dependency resolves from a released Stackage snapshot. To
depend on it from another `stack` project in the meantime, add it as an
`extra-dep` pointing at this repository in your `stack.yaml`, e.g.:

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

Build the software by executing

```
$ stack build
```

Run the tests by executing

```
$ stack test
$ stack run -- test
```

## Development

`-Werror` is **not** part of the default build — it's gated behind a cabal
flag (`werror`, off by default) so that a warning newly introduced by a
future GHC release can't break a downstream consumer's build. This repo's
own build/test loop turns it on by default via the `Makefile`
(`make all`, `make test`), so warnings still fail locally and in CI. To
turn it on directly with `stack`:

```
$ stack build --flag incremental-computations:werror
$ stack test  --flag incremental-computations:werror
```

## A minimal worked example

The following is the complete demo at
[`app/Control/Computations/Demos/Simple/Main.hs`](app/Control/Computations/Demos/Simple/Main.hs)
— a self-contained program that compiles against the library's public API
(nothing here reaches past `Control.Computations.CompEngine` and the shipped
`FlowImpls`). It defines two computations chained together — one that counts
the lines in a file, one that sums a list of such counts — and wires them to
a `FileSrc`/`FileSink` pair plus the built-in `IOSink` for logging.

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

-- Build the FileSrc/FileSink instances once, up front, then drive the
-- wired computation graph forward until it settles. Each instance's name
-- ("fileSrc"/"fileSink") is written down exactly once, right where the
-- instance is built -- wireAllComps and withCompFlows both derive their
-- ids from these same instances (via typedCompSrcIdOf/typedCompSinkIdOf),
-- so there is no second copy that could drift out of sync.
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

Run it (as the `simple` subcommand of the demo executable — see below):

```
$ stack run -- --log-level info simple
```

## Demo: Directory sync

This demo synchronizes the content of one directory to another. The
sync works recursively and continuously.

[Source code](app/Control/Computations/Demos/DirSync)

To start the sync from directory `A` to directory `B`, execute the following command.
Note that `A` must exist, `B` is created automatically.

*Attention: everything in B that does not exist in A will be deleted!*

```
$ stack run -- --log-level info sync --source A --target B
```

## Demo: Hospital

This demo shows how a processing pipeline for hospital data might look like.
The demo is very basic. It supports admission and discharge of patients, as well
textual notes being added to a patient. A simulation creates a stream of events
driving the demo.

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

Its scale is configurable via the `PERSIST_BENCH_SCALE` environment
variable — a float multiplier applied to the synthetic dependency graph's
size (defaults to `1.0`, a ~1,000,000-instance run). For a quicker,
~100,000-instance run:

```
$ PERSIST_BENCH_SCALE=0.1 stack bench
```

See [`docs/benchmark-notes.md`](docs/benchmark-notes.md) for working notes
and measured numbers from prior runs of this benchmark; it isn't polished
documentation, but it's useful context if you're trying to reason about the
engine's performance characteristics.

## Known issues

None currently blocking a release. Every dependency resolves from the
`lts-24.51` snapshot, so `stack.yaml` needs no `extra-deps` and the package
builds from an sdist tarball on its own.
