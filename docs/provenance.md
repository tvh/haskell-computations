# Where these ideas come from

Research notes tracing the intellectual lineage of this engine and of the
optimization work in [`benchmark-notes.md`](benchmark-notes.md). Sourced
where possible from primary papers and from the FUNARCH '23 paper's own
related-work section (the PDF in the repository root); inference is marked
as such.

Three separate traditions converge here. The engine sits at the
intersection of the first two; the optimization campaign pulled in the
third.

---

## 1. Self-adjusting computation (the model)

**Umut Acar and colleagues, CMU, 2002–2009.** The root idea: record an
execution as a *dynamic dependence graph* (DDG), then, when an input
changes, re-run only the parts of the computation that actually depended on
it — "change propagation" — rather than recomputing from scratch. Acar's
2005 thesis adds memoization to the DDG and analyses *trace stability*, the
property that determines when this beats recomputation asymptotically.

- Acar, Blelloch, Harper. *Adaptive Functional Programming.* (Wehr's [2])
- Acar. *Self-Adjusting Computation.* PhD thesis, CMU-CS-05-129, 2005.
- Acar, Blelloch, Blume, Tangwongsan. *An Experimental Analysis of
  Self-Adjusting Computation.* PLDI 2006. (Wehr's [1])

**Where this engine departs.** The FUNARCH paper is explicit: Acar's line
tracks dependencies at fine granularity — "self-adjusting algorithms on
lists react on deletions and insertions of individual list elements" —
whereas "our framework also maintains a dynamic dependence graph, but this
graph records dependencies *between computations*. Hence, our approach is
much more coarse-grained."

That single design decision is what makes the whole optimization campaign
possible. Coarse granularity is why a node can be a fat record with hashes
and edge arrays, and why 1M nodes is the interesting scale rather than 1M
list cells.

### The demand-driven critique: Adapton

**Hammer, Khoo, Hicks, Foster. *Adapton: Composable, Demand-Driven
Incremental Computation.* PLDI 2014.** Two objections to classical SAC:
recomputation is *oblivious to demand* (changed inputs propagate even to
outputs nobody is observing), and SAC's global linear order over
computations blocks reuse across reordered or shared subcomputations.

Adapton matters here for a reason that only shows up in section 3 — it is
Salsa's acknowledged ancestor, so the columnar storage design traces back
to the same root by a different path.

---

## 2. Build systems (the vocabulary, and the implementation)

**Neil Mitchell. *Shake Before Building.* ICFP 2012.** (Wehr's [16].) The
paper names Shake as a direct implementation influence, and the
correspondence is structural, not vague:

| this engine | Shake |
|---|---|
| `CompM` (computation body) | `Action` |
| `CompWireM` (wiring) | `Rule` |
| computation params/results | keys/values |

Two monads — one for running a step, one for declaring the graph — is
Shake's shape.

**Mokhov, Mitchell, Peyton Jones. *Build Systems à la Carte.* ICFP 2018 /
JFP 30, 2020.** (Wehr's [17].) This is where the *vocabulary* in
`benchmark-notes.md` comes from. The paper decomposes any build system into
two orthogonal choices — the **scheduler** (what order) and the
**rebuilder** (whether to rebuild) — and classifies Make, Shake, Bazel,
Nix, Excel, Buck and CloudBuild in one table.

Wehr classifies this framework in exactly those terms: **dynamic
dependencies, early cutoff, a suspending scheduler, and — with
`fullCaching` — a minimal rebuilder.**

Terms this repo inherits from that paper without always saying so:

- **early cutoff** — stop propagating when a rerun produces an unchanged
  result. The README describes it; `fullCaching`'s result hash implements
  it.
- **verifying traces** vs **constructive traces** — the distinction behind
  `hashCaching` (store only hashes, recompute values) versus `fullCaching`
  (store the value). The Rust port's open-items list uses "verifying
  traces" as a term of art; this is its source.
- **suspending scheduler** — why `CompM` suspends mid-body on a dependency
  rather than pre-declaring its dependencies.

---

## 3. Functional reactive programming (the sibling, not the parent)

The paper devotes a section to FRP and lands on *related but distinct*:

- Elliott & Hudak. *Functional Reactive Animation.* ICFP 1997. (Wehr's [9])
- Elliott. *Push-Pull Functional Reactive Programming.* 2009. (Wehr's [8])
- Cooper & Krishnamurthi. *FrTime.* (Wehr's [5])
- Bainomugisha et al. reactive programming survey. (Wehr's [3])

Differences the paper draws: no *behaviors* (continuously time-varying
values) — all state changes are discrete; computations are first-order;
lifting is explicit because `CompM` is a type barrier.

**One inheritance worth naming: glitches.** A glitch is an update
inconsistency where part of the program sees new values while another part
still sees old ones. FrTime prevents them by imposing a topological order.
This framework does *not* — the paper states plainly that glitches are
possible, because computations may read new values from sources before
those values have propagated. The "glitches during propagation are
possible" caveat in `benchmark-notes.md` is not a limitation we discovered;
it is an accepted trade inherited from the design.

---

## 4. The monad (`CompM`)

### Concurrency without fork

**Simon Marlow, Louis Brandy, Jonathan Coens, Jon Purdy (Facebook). *There is
no Fork: an Abstraction for Efficient, Concurrent, and Concise Data Access.*
ICFP 2014** (Haxl).
[PDF](https://simonmar.github.io/bib/papers/haxl-icfp14.pdf),
doi:[10.1145/2628136.2628144](https://doi.org/10.1145/2628136.2628144),
pp. 325–337. Author list and title verified against the PDF's title page; DOI
and pagination cross-checked against dblp (ACM also carries it under the
SIGPLAN Notices DOI prefix `10.1145/2692915.2628144`).

The insight: represent a computation as either `Done` or `Blocked` on a set of
requests, and let the *applicative* combinator explore both branches so
independent requests batch automatically — no explicit fork. Verified verbatim
in the paper (§3.1):

```haskell
data Fetch a = Done a | Blocked (Fetch a)
```

`CompM`'s `CompFinished`/`CompSuspended` is that shape, and `compMAp`
running both sides to combine two suspensions into one `CompReqCombined` is
Haxl's batching. Stage 2 of the campaign then adopted Haxl's *other* lesson
— `GenHaxl` is a monad over `IO` with mutable state — to delete the
per-bind dependency-set union.

### Making binds cheap

A naive free/resumption monad degrades quadratically under left-nested
binds. Three papers converge on the fix, and this codebase cites the third
directly in `Types.hs`:

- van der Ploeg & Kiselyov. *Reflection without Remorse.* Haskell 2014 —
  type-aligned sequences; names the problem in iteratees, LogicT, free
  monads and extensible effects.
- Kiselyov & Ishii. *Freer Monads, More Extensible Effects.* Haskell 2015 —
  the `FTCQueue`.
- **Jaskelioff & Rivas. *A Smart View on Datatypes.* ICFP 2015** — the one
  `ContCompM` cites by name.

The deeper root is the codensity transform, and before that Hughes'
difference lists: represent a sequence by its composition so appending is
O(1) regardless of association. Both have real citations, so this is not
folklore:

- Hughes. *A novel representation of lists and its application to the
  function "reverse".* Inf. Process. Lett. 22(3), 1986 — difference lists.
- **Voigtländer. *Asymptotic Improvement of Computations over Free Monads.*
  MPC 2008, LNCS 5133, pp. 388–403** —
  [PDF](https://janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf),
  doi:[10.1007/978-3-540-70594-9_20](https://doi.org/10.1007/978-3-540-70594-9_20).
  This is the paper that shows the codensity monad reduces free-monad
  complexity from quadratic to linear — the result the three papers above
  are all reaching for by different routes.

The equivalence between the two is also a stated result rather than a vibe:
the codensity transform and difference lists are both instances of the
Cayley representation of monoids in a monoidal category. Kmett's
*Free Monads for Less* series is the accessible Haskell write-up of the same
material.

### The path not (yet) taken

- Felleisen, 1988 — delimited control operators, prompts.
- Danvy & Filinski. *Abstracting Control.* LFP 1990 — `shift`/`reset`.
- → Alexis King, GHC proposal 313, `prompt#`/`control0#`, **landed in GHC
  9.6** — the machinery behind `eff` and "Effects for Less".

`benchmark-notes.md` records that this was unavailable until the compiler
upgrade (the project was on GHC 9.4.5), and is now open.

---

## 5. Storage layout (the optimization campaign)

### Salsa — and the loop back to Adapton

The Rust port's columnar "ingredient" tables were blueprinted on
[Salsa](https://github.com/salsa-rs/salsa), whose README states it is
"inspired by **adapton**, glimmer, and rustc's query system," crediting
Eduard-Mihai Burtescu, **Matthew Hammer** (Adapton's author), Yehuda Katz
and Michael Woerister.

So the lineage closes a loop: Acar's SAC → Adapton's demand-driven critique
→ Salsa → the Rust port's Tier 2 → this repo's Stage 3 columnar rewrite.
The storage design arrived back at a codebase whose *model* came from the
same root via Shake and the build-systems branch.

Salsa's **red-green algorithm** (from rustc's incremental compilation) is
early cutoff by another name: a node marked potentially-dirty stops
invalidating downstream once a recomputed input turns out unchanged.

### Interning is 1958 technology

`SrcDepIntern`/`KeyIntern` are **hash consing**: canonicalize structurally
identical values to one shared representative, then compare and store by
identity. First described by **A. P. Ershov in 1958** — common
subexpression elimination for the BESM machine — and named in 1970s Lisp
implementations reusing `cons` cells (Goto, 1974). Shake interns keys to
`Id` (an `Int`); Bazel's Skyframe does the same.

Stage 4g's refcounting is the classic reclamation half of the technique:
hash-consing tables that never release entries leak, which is why the
literature pairs them with weak references or explicit counts.

### CSR

The per-def edge arenas (Stage 4b) are **compressed sparse row** format,
from 1960s–70s numerical linear algebra: store all non-zeros contiguously
plus a per-row offset array. Applying it to a dependency graph rather than
a matrix is the standard adjacency-list-as-CSR representation used
throughout graph processing.

### Eviction

turbo-tasks/Turbopack's reported biggest memory win — evicting in-memory
cache because a persisted copy exists — is the blueprint for the
unimplemented Tier 3. The engine's own types already anticipate it:
`CapMetaCached` vs `CapValueCached` is the verifying/constructive-trace
distinction from §2, and demoting one to the other at runtime *is*
eviction.

---

## What isn't borrowed

Two things in this arc don't trace to a source I could find:

- **The "load anyway" persistence design** (Rust port, Stages 0–2): load a
  stale snapshot even when the binary fingerprint mismatches, mark
  everything `Revalidate` in a background priority tier, and let
  genuinely-changed inputs preempt in a foreground tier. Build systems
  discard on mismatch; this serves stale results until proven wrong.
- **The coarse-grained/fine-grained split itself** as a deliberate
  *architectural* choice rather than an implementation compromise — which
  is the FUNARCH paper's actual contribution.

## Wehr's bracketed references, resolved

Every `[n]` used above, transcribed from the FUNARCH '23 preprint's
bibliography (`Wehr_A-Software-Architecture-Based-on-Coarse-Grained-Self-Adjusting-Computations.pdf`
in the repo root):

| ref | citation |
|---|---|
| [1] | Acar, Blelloch, Blume, Tangwongsan. *An experimental analysis of self-adjusting computation.* PLDI 2006. doi:[10.1145/1133981.1133993](https://doi.org/10.1145/1133981.1133993) |
| [2] | Acar, Blelloch, Harper. *Adaptive functional programming.* TOPLAS 28(6), 2006. doi:[10.1145/1186632.1186634](https://doi.org/10.1145/1186632.1186634) |
| [3] | Bainomugisha, Carreton, Van Cutsem, Mostinckx, De Meuter. *A survey on reactive programming.* ACM Comput. Surv. 45(4), 2013. doi:[10.1145/2501654.2501666](https://doi.org/10.1145/2501654.2501666) |
| [5] | Cooper, Krishnamurthi. *Embedding Dynamic Dataflow in a Call-by-Value Language.* ESOP 2006, LNCS 3924. doi:[10.1007/11693024_20](https://doi.org/10.1007/11693024_20) — this is **FrTime** |
| [8] | Elliott. *Push-pull functional reactive programming.* Haskell Symposium 2009. doi:[10.1145/1596638.1596643](https://doi.org/10.1145/1596638.1596643) |
| [9] | Elliott, Hudak. *Functional Reactive Animation.* ICFP 1997. doi:[10.1145/258948.258973](https://doi.org/10.1145/258948.258973) |
| [14] | *Incremental — Library for Incremental Computations.* Jane Street, 2023. <https://opensource.janestreet.com/incremental> — the OCaml SAC implementation Wehr points at |
| [16] | Mitchell. *Shake before building: replacing make with Haskell.* ICFP 2012. doi:[10.1145/2364527.2364538](https://doi.org/10.1145/2364527.2364538) |
| [17] | Mokhov, Mitchell, Peyton Jones. *Build systems à la carte: Theory and practice.* JFP 30, 2020. doi:[10.1017/S0956796820000088](https://doi.org/10.1017/S0956796820000088) |
| [21] | *Racket, the Programming Language.* <https://racket-lang.org> |

NOTE: [17] is the extended **JFP 2020** version. The ICFP 2018 paper of
nearly the same name is a different artifact; cite whichever you actually
read, and don't silently swap the venue.

Two things the resolution turned up that are worth knowing:

- **[14] is Jane Street's `Incremental`.** Wehr's related-work sentence
  "There exists an OCaml implementation for self-adjusting computations [14]"
  points at a production library, not a paper. That is the closest thing to
  an industrial cousin of this engine.
- **The Haxl author line in §4 was wrong** before this pass — it read
  "Marlow, Peyton Jones, Kmett, Mokhov", three of whom had nothing to do
  with the paper. Corrected against the PDF's title page and now carries the
  verified `data Fetch a = Done a | Blocked (Fetch a)` line as well. This is
  a good argument for resolving references instead of recalling them.
