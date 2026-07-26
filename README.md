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

[`docs/benchmark-notes.md`](docs/benchmark-notes.md) has measured numbers
from prior runs.

NOTE: those are working notes, not polished docs.
