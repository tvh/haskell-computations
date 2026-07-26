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

Sources and sinks are pluggable: the library ships flow implementations for
files (`FileSrc`/`FileSink`), for content-addressed blob storage
(`FileStoreSink`), for in-memory maps (`HashMapFlow`), for plain `IO` actions
(`IOSink`), for periodic ticks (`TimeSrc`), and for structured logging
(`CompLogging`). You can also implement your own by instantiating the
`CompSrc`/`CompSink` classes.

## Installing

This package is not yet on Hackage (see [Known issues](#known-issues)
below). To depend on it from another `stack` project, add it as a
`extra-dep` pointing at this repository in your `stack.yaml`, e.g.:

```yaml
extra-deps:
  - github: tvh/haskell-computations
    commit: <commit-sha>
```

and add `incremental-computations` to your package's `dependencies:` in
`package.yaml` (or `build-depends:` in a hand-written `.cabal` file).

## Building this repository

Requirements: you need the build tool `stack` (https://docs.haskellstack.org/en/stable/).
I use version 2.9.3, I guess a slightly older version might work was well.

Build the software by executing

```
$ stack build
```

Run the tests by executing

```
$ stack test
$ stack run -- test
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

import Data.Proxy
import Prelude hiding (readFile, writeFile)

-- A source and a sink, each identified by a typed, named id.
fileSrc :: TypedCompSrcId FileSrc
fileSrc = typedCompSrcId (Proxy @FileSrc) "fileSrc"

fileSink :: TypedCompSinkId FileSink
fileSink = typedCompSinkId (Proxy @FileSink) "fileSink"

-- A computation from a file path to the number of lines in that file.
numberOfLinesCompDef :: CompDef FilePath Int
numberOfLinesCompDef =
  defineComp "numberOfLines" fullCaching $ \p -> do
    string <- compSrcReq fileSrc (ReadTextFile p)
    pure (length (lines string))

-- A computation that reads a file of paths (one per line) and sums the
-- line counts of each, by calling back into another computation.
sumCompDef
  :: Comp FilePath Int
  -> CompDef FilePath Int
sumCompDef c = defineComp "sum" fullCaching $ \p -> do
  string <- compSrcReq fileSrc (ReadTextFile p)
  list <- mapM (evalCompOrFail c) (lines string)
  pure (sum list)

-- The top-level computation: run the sum over a fixed file and write the
-- result out through the sink.
storeCompDef :: Comp FilePath Int -> CompDef () ()
storeCompDef c = do
  defineComp "store" fullCaching $ \() -> do
    i <- evalCompOrFail c "file_list.txt"
    compSinkReq fileSink (WriteTextFile "output.txt" ("number of lines: " ++ show i))

-- Wiring: turn each CompDef into a runnable Comp, chaining them together.
wireAllComps :: CompWireM (Comp () ())
wireAllComps = do
  numberOfLinesC <- wireComp numberOfLinesCompDef
  sumC <- wireComp (sumCompDef numberOfLinesC)
  wireComp (storeCompDef sumC)

-- Register the concrete flow implementations for the source/sink ids used
-- above, then hand control to the given action.
withCompFlows :: FilePath -> CompFlowRegistry -> IO () -> IO ()
withCompFlows tgt reg action =
  withFileSrc (defaultFileSrcConfig "fileSrc") $ regSrc reg $ do
    fileSink <- makeFileSink "fileSink" tgt
    registerCompSink reg fileSink
    registerCompSink reg ioSink
    action

-- Drive the wired computation graph forward until it settles.
main :: IO ()
main = withSysTempDir $ \tgt -> do
  logNote ("Target directory: " ++ tgt)
  compDriver (withCompFlows tgt) wireAllComps ()
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

- **`large-hashable` is not on Hackage.** `stack.yaml`'s `extra-deps` pulls
  it from a git commit rather than a Hackage release. Until it (or a
  replacement) is available from Hackage, this package cannot be uploaded to
  Hackage with `cabal upload`, even though it builds and tests cleanly from
  an sdist tarball otherwise.
