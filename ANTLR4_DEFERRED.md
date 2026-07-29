# ANTLR4 translator — deferred minor issues

Tracked for later attention once the full debug build compiles. None of these
block compilation; they are correctness-of-match or robustness refinements.

## Cleanup: normalise procedure-name CASE across definition and call sites

**Goal (Dylan):** find every procedure whose **definition case differs from its call-site
case** (or where call sites disagree among themselves) and make them consistent. Foo/Fortran
are case-insensitive so these compile and run fine, but the inconsistency is annoying and
trips case-sensitive tooling.

**Why it matters (concrete):** discovered during phase B (dead-code elimination). `textfile.foo`
defines `reset_IO_status` (upper `IO`) but `vec{basis}.foo` calls it as `stdin.reset_io_status`
(lower). The dead-code analysis keyed its call-graph nodes case-sensitively, so the call didn't
match the definition and the procedure was wrongly pruned — a latent, silent trap. (Worked
around in the translator by lower-casing the method part of every graph node via `node()`;
this cleanup would remove the underlying inconsistency in the *sources*.)

**How to tackle (parse-tree driven, reuse phase-B infra):** the translator already walks every
`ProcDef` (definitions) and every `PostfixContext` (calls). Add a `--case-report` mode that
records, per lower-cased procedure name, the **set of distinct spellings** seen across its
definition header and all call sites; flag any name with >1 spelling, listing file:line of each
variant. Then normalise — the definition's spelling is the natural canonical form — and rewrite
the call sites (a targeted, parse-tree-driven edit like `--add-self-intent`, NOT a blind sed,
so commented-out and string-literal occurrences are left alone). Related: [[submodule-call-autoresolution-done]]
already hit a case bug in the submodule registry (commit 627db872); this is the same family.

## Closed (won't-do): eliminate explicit `TYPE:proc` calls — at its practical limit

**What:** the submodule-call cleanup (`4cd995df`) auto-resolved `.SUBMOD:proc` etc., but
explicit **type-qualified** calls `TYPE:proc` / `TYPE::proc` (e.g. `GAUSSIAN_DATA:...`,
`STR:...` for namespace access + method calls) were left **qualified on purpose**.

**Why it's not just a mechanical `TYPE:proc(x)` → `x.proc` rewrite:** that transform is
**unsafe** and was tried and reverted. An **elemental** method invoked on a `VEC{T}` array
receiver resolves against the *receiver's* type (`VEC{T}`), not the element type `T`, so
`x.proc` can bind a different (array-level) overload than `T:proc` intended — this introduced
a `use` cycle. See memory `typeproc-elemental-array-hazard`. A correct elimination needs
**type-aware** resolution that respects elemental/array-receiver semantics (and the
GAUSSIAN_DATA namespace-access case), not a blind receiver swap. Low priority — the explicit
form compiles and runs fine; this is a consistency/readability cleanup, not a correctness bug.

**Analysis tool built — `--type-qualified-call-report` (read-only).** Rather than rewrite
blindly again, `FooToFortran` now classifies every site by asking the *real* resolver two
questions — "where does `TYPE:proc` resolve today?" vs. "where would `arg1.proc(...)` go?" —
and only calls a site **SAFE** when both answers exist and agree. Writes
`type_qualified_calls.tsv` (per-site verdict + why). New helpers `buildElementalByModule`,
`runTypeQualifiedCallReport`, `classifyTypeQualifiedCall`, `isLonePostfixArg`. Emission is
untouched; it just walks the modules and throws the Fortran away (~11 min single-JVM full walk).

**Verdict: this task is at its practical limit — it cannot be cheaply or completely done.**
Report over **2068** sites (2026-07-29):

| class | count | % | convertible? |
|---|---|---|---|
| NAMESPACE (989 = `GAUSSIAN_DATA::…`) | 1002 | 48.5% | no — data access, no receiver to promote |
| MODULE_MISMATCH | 506 | 24.5% | no — would bind a *different* module |
| EXPR_RECEIVER | 232 | 11.2% | no — receiver needs added parens |
| ELEMENTAL_HAZARD | 99 | 4.8% | no — the exact trap that sank the reverted attempt |
| **SAFE** | **154** | **7.4%** | **yes — the only candidates** |
| UNKNOWN / BY_NAME / COMPONENT_COLLISION / NO_RECEIVER | 75 | 3.6% | no — untypeable or non-call |

Only **7.4%** is mechanically convertible; the other **~92%** each fail for a *structural*
reason (namespace access with no receiver, genuine cross-module dispatch, expression receiver,
or elemental/array-receiver semantics) that no blind rewrite can satisfy. Converting the 154
SAFE sites would be a large, file-spanning diff for a cosmetic gain while **leaving the bulk of
the explicit forms in place anyway** — so the readability payoff never actually lands. The
explicit `TYPE:proc` form compiles, runs, and is unambiguous. **Recommendation: stop here.**
Keep the report as the durable evidence and the work-list should anyone ever want the SAFE
subset; treat full elimination as *not worth doing*, not merely *not yet done*.

## DONE: phase B — per-executable dead-code elimination

**Goal (Dylan):** eliminate code dead for a specific executable (e.g. `run_molecule`/`tonto`),
in a separate build dir, without affecting the other executables or the normal build.

**Delivered** in `FooToFortran.java` + `CMakeLists.txt`:
- `--call-graph-report` → Graphviz `call_graph.dot` / `module_use.dot` (submodules collapsed to
  parent) / `submodule_use.dot`; `--dead-code-report <root.foo>` → per-module live/dead TSV;
  `--purge-dead-code <root.foo>` → two-pass emit dropping unreachable procs. CMake exposes the
  `callgraphs` target and `-DPURGE_DEAD_CODE=<stem>` (separate build tree). See README §7b, CLAUDE.md §8.
- Reachability = BFS from the root program's entry calls over a call graph captured by
  piggybacking on the real call-resolution. `TYPES`/`SYSTEM` (wholesale-`use`) never pruned.

**Validated:** `-DPURGE_DEAD_CODE=run_molecule` release build compiles 0-error, ~32% of the
~7600 procedures dropped, binary 33→25 MB, and ctest is **121/124 — identical to the full build**
(same 3 known-bad). Three reachability-analysis bugs were caught only by the compile+test gate,
each a call form that bypassed the `use`-based capture: (1) same-module `::proc`/bare-selfless
calls (fixed: `recordSelfCall`); (2) case-sensitive node keys (`reset_IO_status` vs
`reset_io_status`; fixed: `node()` lower-cases the method part — motivates the case-cleanup goal
above). CPP-macro-hidden calls all target `SYSTEM` (always kept), so no macro-root class exists.

## DONE: simplify the DOT call-graph output

**Goal (Dylan):** reduce the complexity of the graphs from `--call-graph-report` (phase B).
`call_graph.dot` is a **procedure-level** graph — ~7600 nodes / ~24k edges — too dense to read
as a single image. `module_use.dot` (921 edges) was legible-ish but still a hairball.

**Delivered** as `scripts/simplify_callgraph.py` (a post-processor over `module_use.dot`, not a
translator change — the DOT already carries every `use` edge):
- **Aggregate** — fold module families into one coloured node: `NUMBERS` (INT/REAL/CPX),
  `ARRAYS` (VEC/MAT of primitives), `SHELLS`, `GAUSSIANS`, `MAPS`, `ISOSURFACES`; and re-point
  `VEC{T}`/`MAT{T}` over a derived type to that element's module (`VEC_ATOM`→`ATOM`), surfacing
  real deps like `MOLECULE→ATOM`. Dropped dead `BREAKDOWN_DATA`, `MULTI_T_ADP`.
- **Ambient** (`--simplify`) — hide the 7 universal utilities that take 62% of all edges
  (`NUMBERS ARRAYS STR BIN TEXTFILE BUFFER TABLE_COLUMN`). 139/921 → **50 nodes / 114 edges**.
- **`--module NAME`** — documentation ego-graph of one module's direct dependencies
  (`--reverse` dependents, `--both`); no `concentrate`, so every direct edge shows.

**Key findings, recorded in `docs/CALL_GRAPHS.md`:** the "aggregate vs ambient" distinction
(merge-and-keep vs hide); `concentrate=true` is *lossy* (drops a direct edge parallel to a
longer path — e.g. `ATOM→INTERPOLATOR`), so the doc mode avoids it; and Graphviz has **no** edge
hops/bridges and is already hierarchical, so fewer edges beats a different engine. README §7b
points to the tool + doc. (The procedure-level `call_graph.dot` remains dense — a module-level
*call* graph in `writeDotFiles` is still a possible future refinement, but not needed now.)

## Future task: introduce Fortran-2008 `submodule` constructs

**Goal (Dylan):** use real Fortran-2008 `submodule` where appropriate. **Concept clash to note
first:** a Foo "submodule" (e.g. `molecule.base.foo` → `module MOLECULE.BASE`) currently
translates to a **separate, standalone Fortran module** `MOLECULE_BASE_MODULE`, `use`d like any
other — NOT an F2008 `submodule`. F2008 `submodule (PARENT) NAME` would instead let the 19
`MOLECULE.*` pieces share one parent interface and break the `use`-graph coupling (a submodule
sees its ancestor's specification without a `use`, and changing a submodule body doesn't force
recompilation of the parent's users). Investigate whether mapping Foo submodules onto F2008
submodules simplifies the emitted module graph and compile-time dependencies. Touches
`emit()`/`buildUseFile()`/`buildInterfaceFile()` and the module-naming scheme.

## Future task: split `types.foo` into several modules (parallel compilation)

**Goal (Dylan):** `types.F90` is the slowest single compile in the build and it is a
**serial bottleneck** — everything `use`s it, so nothing else can start until it finishes.
Split `types.foo` into several independent modules so they compile in parallel under `-j`.

**Why it is slow — measured (2026-07-28, M2 Pro / Tahoe, gfortran-14):**

| Stage | Time |
|---|---|
| parse + `.mod` generation (`-fsyntax-only`) | 0.01 s |
| `-O0` (codegen only) | 29 s |
| `-O1` | 45 s |
| full release flags (`-Ofast … -O2`) | 839 s (~14 min) |

The front end is *instantaneous*, so the ~90 derived-type definitions themselves cost nothing.
The cost is **codegen**: those types have ~585 allocatable/pointer components, and gfortran
auto-generates a deep-copy helper per type (`__copy_types_module_<TYPE>`) — 167 text symbols and
**8.9 MB of `__TEXT`** from 5.7 k lines containing *zero* user-written procedures. Optimising
that generated boilerplate is what costs the four minutes.

**Interim fix already applied:** `types.F90` is compiled at `-O1`
(`set_source_files_properties` in `CMakeLists.txt`), 839 s → 45 s (~18x), no runtime cost worth
measuring (the helpers are memcpy-shaped). The serial-bottleneck problem remains.

**Note — F2008 `submodule` does NOT help here** (cf. the submodule task above): type
definitions are part of a module's *interface* and cannot live in a submodule, and the
`__copy_*` helpers are generated where the types are defined. Splitting into several **real
modules** is what parallelises; submodules only avoid recompilation cascades.

**Care needed:** the split must respect the derived-type dependency order (types with
components of other derived types), and every `use TYPES_MODULE` site plus the translator's
`.use`-file generation must follow. Check whether the translator can emit the split
automatically from one `types.foo` rather than requiring the source be broken up by hand.

## RESOLVED (was: atomic (guess) SCF fails for partly-filled degenerate shells)

**There was no SCF instability.** The diverging boron atomic SCF found on 2026-07-28 was a
*symptom* of the gfortran miscompilation of `shell1quartet.F90` (see "verify the macOS build"
below): the two-electron integrals were wrong, so the atomic SCF was iterating on a corrupted
Fock matrix. With the compiler workaround in place boron converges normally, the `DIE` does not
fire, and rgbi/BN reproduces the Linux reference **exactly**. Nothing here needs fixing in the
SCF or in the physics.

**Kept, because they stand on their own merits:**

- **`make_ANOs` DIEs on a non-converged atomic SCF** (`molecule.scf.foo`). It previously
  collected the per-atom `converged` flag, printed it only in verbose mode, and carried on —
  so ANOs built from a garbage iterate silently corrupted every quantity derived from the
  atomic projectors while molecular totals still looked plausible. Refusing to continue is
  right regardless of what caused the non-convergence, and it is exactly the check that would
  have surfaced this class of fault sooner.
- **The widened guess-SCF whitelist** (`molecule.set.foo`). Only `output`, `initial_MOs`,
  `initial_density`, `convergence`, the DIIS tolerance and `relativity_kind` were copied from
  the parent, so `max_iterations`/damping/level-shift given in the input were silently
  discarded and a failing guess SCF could not be rescued from the input file at all.

**Retired follow-ups.** Two items were queued here — calibrating "sensible" guess-SCF recovery
options, and adding a tiny symmetry-breaking charge to make the atomic SCF deterministic. Both
were motivated purely by the phantom instability and are **no longer needed**. Worth keeping
from that discussion, though: the textbook remedy of spherically-averaged *fractional*
occupations does **not** apply here, because UHF requires integer occupancy of spin orbitals —
so if a genuine degeneracy problem ever does appear, the degeneracy must be *lifted*, not
averaged over.

**The cautionary tale.** Every hypothesis below was tested against the real code and refuted,
one after another. In hindsight they all failed for the same reason: they were searching for a
*physics* cause of a *compiler* artefact. The tell was there early — the same input gave
different answers on different platforms — and it deserved more weight than it got.

| Hypothesis | Verdict |
|---|---|
| Boron's atomic SCF diverges; ANOs built from it anyway | **Observed** (100 iters, −6…−69 Ha, "DIIS stuck", final −65 Ha for an atom whose energy is ≈ −24.5) |
| That causes BN's platform-dependent Roby numbers | **Demonstrated by intervention** — forcing convergence moved B 5.35/3.40 → 6.50/6.63, collapsed the cross-library spread 1.95 → 0.13, landing on the Linux reference 6.48 |
| It explains the other rgbi failures | **Refuted** — only BN fails to converge; C2/CN-/CO/F2/N2/NF/O2 converge with bit-identical atomic energies across LAPACKs and still deviate |
| "Partly-filled degenerate shell" predicts which atoms fail | **Refuted** — carbon 2p² is partly-filled and degenerate, and converges |
| Low nuclear charge (B, Z=5) | **Not supported** — standalone UHF SCFs *converge* for B(5), C(6), N(7), Al(13), Si(14). But see the oxygen-atom finding below: standalone atomic energies on this Mac are themselves wrong, so these runs test convergence only, not correctness |
| Bad initial guess | **Refuted** — `initial_density=` core / fock / promolecule / progroup all give identical results and all fail (verified the option reaches the SCF: "Kind of initial density ... promolecule") |
| Loose guess tolerances (1e-3 vs 1e-5) | **Refuted** — standalone boron converges at every combination of `convergence`/DIIS tolerance |
| Wrong atom setup in the guess | **Refuted** — guess reports B, charge 0, multiplicity 2, 5 e⁻ (3α/2β), all correct |

**Do not trust the numbers in that table as physics.** Everything above was measured on
*miscompiled* binaries, so quantities like the "two converged boron solutions 0.57 Ha apart"
(−24.3378 vs −23.7663) are artefacts of wrong two-electron integrals, not evidence of an SCF
pathology or of basin-hopping. They are kept only as a record of what was tried.

One observation from that work is still true and worth knowing, since it is a property of the
defaults rather than of the bug: the guess SCF withdraws **all** stabilisation at exactly the
iteration DIIS takes over —

```
level shift 0.30, quits at 3   |   damping 0.50, quits at 3   |   DIIS extrapolates from 3
```

That is fine when the first three iterations settle, and unforgiving when they do not. It did
not cause this failure, but it is a thin margin for any genuinely hard atom.

### Retired: (a) sensible guess-SCF recovery options, (b) symmetry-breaking charge

Both were queued to tame an instability that turned out not to exist. The `DIE` message no
longer prescribes any values (it names the relevant keywords only), so nothing extreme is
enshrined, and the tiny-charge idea has no problem left to solve. The `SCF_DATA` machinery it
would have used (`.cluster_charges`, `.cluster_charge_positions`,
`set_using_cluster_charges()`) is noted here only in case a genuine degeneracy problem ever
does turn up.

**Related defect, still open and unrelated to the above:** a `DIE` leaves the process **exit
status 0**. `scripts/test.py` uses `subprocess.check_call`, so a hard error is not detected as
a run failure -- only the output diff catches it. Worth fixing so failures are unambiguous to
CI.

## Future task: test the MPI parallel build

**Goal (Dylan):** verify the MPI build works and its tests pass. Build flags exist
(CLAUDE.md §4: `-DCMAKE_Fortran_COMPILER=mpifort … -DMPI=1`, optional `-DNO_ERROR_MANAGEMENT`);
`scripts/test.py` has a `--mpi` path (`mpirun -n 4`), wired via `WITH_MPI` in `tests/CMakeLists.txt`.
Status is **unverified** for the ANTLR4 translator output. Start by building MPI and running
`ctest` under it; expect the parallel macros (`PARALLEL_DO_*`, `PARALLEL_SUM`, `broadcast_` — all
`SYSTEM`/`tonto`-targeted, see `macros.in`) to be the surface area. Compare against a non-MPI run.

## Future task: verify the macOS build (Apple Silicon / Tahoe)

**Goal (Dylan):** confirm whether Tonto builds and passes tests on current macOS, on
**Apple Silicon (M2) with macOS Tahoe (26)** — to be done in a **separate session on a real
Mac** (the main dev box is Linux; macOS cannot be tested from it). A March-2026 README note
claimed *"many failures on the Apple M2 with Tahoe 26.3 — not recommended"*, but Dylan says
that is **very old** and likely stale, so it must be re-checked rather than trusted. Build via
Homebrew (`brew install gcc cmake openjdk python3 gnuplot`; BLAS/LAPACK come from
`Accelerate.framework`), then run `make report`. **Feed the result back into the docs:** update
the README macOS line (currently softened to "via Homebrew; Linux/WSL is the reference platform")
and the `Building on MacOS` wiki page to say what actually works — pass → "supported", or list the
specific failures if any remain.

**RESOLVED (2026-07-28, session on the real Mac: M2 Pro, macOS 26.5.2 Tahoe, Darwin 25,
gfortran-14 / Homebrew GCC 14.3.0, CMake 4.3.3, Java 26).**

**Outcome: the macOS failures were a compiler miscompilation, not a Tonto bug.** gfortran 14.3
on arm64 miscompiles `shell1quartet.F90` (the two-electron integral code) at `-O3`. Pinning
that one file to `-O2 -fno-schedule-insns` (`CMakeLists.txt`, commit `a3ec1b07`) took the suite
from **82/124 to 118/124**:

| Suite | before | after |
|---|---|---|
| short | 33/51 | 49/51 |
| rgbi | 1/13 | 12/13 |
| long | 18/28 | 25/28 |
| cx | 30/32 | 32/32 |
| **total** | **82/124** | **118/124** |

The build itself was always clean, so the March-2026 README note ("many failures on the Apple
M2 — not recommended") is stale in both respects. **README and wiki still NOT updated** —
Dylan's standing instruction; and the 6 residual failures should be understood first (they are
mostly the longstanding small-difference cases already tracked below, plus one structural diff
in `urea_lamaGOET_grown_CIF` with max rel 0 / LDD 0).

The investigation history is kept below because the false trails are worth not re-walking.
**Note especially:** the diverging boron atomic SCF that dominates the "atomic SCF" section
above was a *symptom* of this miscompilation, not an independent defect — with the compiler
workaround in place boron converges and rgbi/BN reproduces the reference exactly. The `DIE`
added in `make_ANOs` is still worth keeping as a safety net, but its motivating example is gone.

- **The build is clean.** Full translate + compile + link, zero errors; only benign warnings
  (clang deployment-version override ×149, a `-F` flag not valid for Fortran, 2 macro
  redefinitions). So the 2026-03 "not recommended" note is stale *as regards building*.
- **`make report`: 82/124 loose** (exact 73, lastdig 79) — short 33/51, rgbi 1/13, long 18/28,
  cx 30/32. Baseline artefacts kept in `/tmp/tonto-mac-baseline/` (tests.log, build log,
  all 44 `stdout.bad`).
- **What passes:** geometry/CIF processing and Hirshfeld surfaces (cx 30/32), and plain SCF
  energies (`h2o_rhf_cc-pVDZ`, `blyp`, `rks_B3LYPG`, `xalpha`, `aug-cc-pVDZ`) — all *exact*.
- **What fails:** downstream *property* evaluation — 1e properties, structure factors / HAR,
  cluster charges, Roby bond indices, polarisabilities/TDHF, Salvador properties.
- **Ruled out (with evidence, not argument):**
  - *Not* the ANO occupancy threshold (`atom.foo`, `count(.NAO_occupations>=1/14)`): scanning
    `occupied_ano_cutoff=` over 0.02–0.30 changes BN not at all (the marginal occupation is
    ~0.3, nowhere near 1/14).
  - *Not* uninitialised heap: identical results under `MallocScribble=1 MallocPreScribble=1`.
  - *Not mainly the LAPACK version.* Accelerate's Fortran LAPACK is frozen at **3.2.1 (2009)**
    (confirmed via `ilaver`); switching to Homebrew OpenBLAS (**3.12.0**) flipped exactly
    **one** short test (33/51 → 34/51) and **zero** rgbi (1/13 either way). Worth doing as
    hygiene — now the macOS default, see `CMakeLists.txt` — but it is not the cause.
- **Leading hypothesis — degenerate eigenspaces.** In rgbi/BN the Roby angle table has an
  exactly degenerate pair (states 7 and 8, both `theta = 68.7340°` — the π_x/π_y pair).
  Eigenvectors within a degenerate eigenspace are **not unique**: any orthogonal mixing is a
  valid eigenbasis, and different LAPACKs return different mixtures. Anything downstream that
  uses individual eigenvectors, rather than the projector onto the whole subspace, is then
  **basis-dependent, i.e. ill-defined** — which explains differences far too large for
  round-off. Strongly supported by the rgbi pattern: every failing molecule is linear/highly
  symmetric (BN, C2, CN-, CO, F2, N2, NF, O2, Ni-carbonyl — all with degenerate π), while the
  **only exact pass is `CHFCl`, the one molecule with no symmetry and no degeneracy**. Three
  environments give three different answers for BN's B population: Linux 6.48, Accelerate 6.16,
  OpenBLAS 6.34.
  **Next step:** relink an Accelerate binary from the same objects (`.o` files are intact) and
  A/B the `output_theta_info= YES` tables. If the *thetas* (eigenvalues, invariant) agree while
  the populations differ, non-invariance under degenerate mixing is confirmed — and the fix is
  in the Roby code, not the build.
- **Still uncontrolled:** `-Ofast` implies `-ffast-math`; and the `release/` tree used here
  carried a **stale cached `-O2`** (a fresh configure gives `-O3`), so opt level differed from
  the Linux reference too. A strict-FP rebuild (`-O2 -fno-fast-math -ffp-contract=off`) is the
  clean way to settle the FP-flags question.
- **Also note:** a `1e_properties` diff shows *total* molecular moments agreeing to 7 digits
  while *atomic partitioned* charges/dipoles differ by 15–17% — totals right, partitioning
  wrong. Consistent with the same "non-invariant use of an arbitrary basis" theme.

### RESOLVED: the oxygen atom converged to a variationally impossible energy

The single best probe found so far, because it is one atom, runs in seconds, and its
correctness can be judged from physics alone rather than by diffing against a reference.

Run `tests/short/oxygen_atom_uhf_cc-pVDZ/stdin` **verbatim**:

| | Total energy |
|---|---|
| Linux reference (`tests/short/.../stdout`) | **−74.7923** |
| this Mac, OpenBLAS 3.12.0 | **−77.6178** |
| this Mac, Accelerate 3.2.1 | **−77.6178** |

Both report "converged". The O atom UHF limit is ≈ −74.81, so **−77.62 lies ~2.8 Ha *below* the
variational limit — impossible for a correct HF calculation.** This is not a tolerance or
round-off effect and not a LAPACK effect (both libraries agree exactly); it is a Mac↔Linux
difference in the SCF itself, so the prime suspects are code generation and `-Ofast`
(`-ffast-math`, FMA contraction, denormal flush-to-zero).

The same signature appears in every standalone atom tried on this Mac (B, C, N, Al, Si all come
out 2–6 Ha too low), and in the *guess* SCF inside BN (its N atom gives −55.7673 where the HF
limit is ≈ −54.40). So it may well underlie a large share of the 42 macOS failures — including
the atomic-partitioning discrepancies — and it is upstream of the ANO/Roby problem rather than
caused by it.

#### Localised: two-electron integrals between *different shells*

Bisection done 2026-07-28 against the Linux reference box (`achari2`, the very machine that
produced the reference outputs — its kernel `6.8.0-88-generic` matches their `Platform:` line).

**Ruled out, each with a direct test:**

| Suspect | Result |
|---|---|
| FP flags | **No** — rebuilt `-O2 -fno-fast-math -ffp-contract=off`: identical (−77.6178, virial 1.9574) |
| LAPACK | **No** — Accelerate 3.2.1 and OpenBLAS 3.12.0 agree exactly |
| Platform `#ifdef`s | **No** — `GNU_gfortran_on_Darwin` is defined but never used anywhere |
| Basis setup / normalisation | **No** — overlap matrix **bit-identical** to Linux, diagonal exactly 1.0 |
| One-electron integrals | **No** — T and V_eN **bit-identical** to Linux (see below) |
| ERI screening | **No** — setting all six `eri_*_cutoff` to 1e-20 changes nothing |
| Initial guess / tolerances | **No** — see the table above |

**Where it actually is.** `V_ee` is not computed directly; `molecule.scf.foo:1868` obtains it *by
subtraction* (`energy − V_NN − V_eN − V_charge − T`), so a `V_ee` discrepancy *is* a total-energy
discrepancy. With V_NN = 0 for an atom and T, V_eN identical, the error is entirely in the
two-electron part of `.SCF_energy`.

**Minimal reproducer — Be atom, STO-3G, milliseconds:**

| Atom (STO-3G) | shells | Mac V_ee | Linux V_ee | T (both) |
|---|---|---|---|---|
| He | one s | 1.055713 | 1.055713 | 2.823526 |
| **Be** | **two s (1s,2s)** | **4.538672** | **4.875821** | 14.844185 |
| Ne | s + p | 54.602059 | 55.508976 | 125.562006 |
| O | s + p | 28.349706 | 29.075205 | 73.444959 |

A **single** shell is exact; **two distinct shells** diverge. So it is not an angular-momentum
(p/d) problem — it is two-electron integrals spanning different shells. The Mac value is always
*too small*, as if contributions are lost.

*Why the densities still agree:* in a minimal basis these atoms have every orbital occupied
(Be: 2 functions / 4 electrons), so the density is fixed by the basis regardless of the Fock
matrix. That is why T and V_eN match while the energy does not — it does **not** imply the
Fock matrix is correct.

**Next step:** dump the individual two-electron integrals for Be/STO-3G on both platforms and
diff them — there are only ~6 unique values. `put_fock_matrix` / `put_density_matrix` print
empty under direct SCF, so this needs a temporary print inserted in the ERI path
(`shell1quartet.foo`, `shell2.foo`, `gaussian4.foo`). Since the one-electron code shares the
same Gaussian machinery and is provably correct, the fault is specific to the ERI routines.

**Repro harness:** `scripts/oxygen_scf_probe.sh <tonto> <repo>` prints the energy decomposition
at truncated iteration counts on either platform.

#### Final: compiler miscompilation of `shell1quartet.F90`

Localised by swapping a single object file: with `shell1quartet.F90` compiled at `-O2` and
everything else untouched, every probe matches Linux exactly (oxygen E/V_ee/V_eN/T/virial;
Be V_ee 4.875821; BN's Roby populations identical to the reference). The path mattered because
only the *engine* ERI route is affected — `use_spherical_basis= T` selects `make_r_JK_direct`
and was an accidental working workaround.

**Evidence it is a GCC bug rather than UB in our source** (not proof — see caveats):

- The generated Fortran is **bit-identical** to Linux's (same md5, 11263 lines), so the
  translator is not implicated.
- Compiles clean under `-Wall -Wextra -Waliasing`: **no warnings at all**.
- Runs clean under `-fcheck=all -finit-real=snan -finit-integer=-999999` with
  `-ffpe-trap=invalid,zero,overflow` armed in the main program: no bounds violations, no
  runtime errors, no traps.
- The one concrete UB candidate — `Jab`/`Jcd` aliasing when `ab == cd`, both `INTENT(INOUT)` —
  is **properly guarded**: every branch in which `same` can be true writes `Jab` only and never
  references `Jcd` (`if (same) ... Jab = Jab + TWO*factor*ev`). Conforming.

*Caveats:* the instrumentation covers this file only (bounds checking is per compilation unit),
`-fcheck` cannot detect argument aliasing at all, and — importantly — the checks necessarily run
on a *correctly* compiled build, since adding them changes codegen enough to make the answer
right. So this shows the source is well-defined when compiled conservatively; it is not a proof
that the failing binary contained no UB.

**No minimal `-O3` workaround exists** (tried, failed): the trigger is not any of the 13 passes
`-O3` adds over `-O2`, and disabling all 13 still gives the wrong answer; nor is it the
vectoriser (`-fno-tree-vectorize`, `-fvect-cost-model=very-cheap`), inlining
(`-fno-inline-functions`, `--param max-inline-insns-auto`), `-fno-ipa-cp-clone`, or
`-fno-strict-aliasing`. There appear to be **two interacting triggers**:

| config | result |
|---|---|
| strict FP, `-O2` | wrong; `-fno-schedule-insns` fixes it |
| fast-math, `-O2` | correct, even with scheduling on |
| fast-math, `-O3` | wrong; `-fno-schedule-insns` does NOT fix it |

Hence the level is pinned rather than a pass disabled. Both switches are applied because that
is the configuration the 118/124 run actually verified.

**Not fixed in GCC 16 (tested 2026-07-29).** A full build with `gfortran-16` (Homebrew GCC
16.1.0) and `-DTONTO_SKIP_ARM64_WORKAROUNDS=ON` compiles cleanly (zero errors) and then
reproduces the miscompilation **exactly** — the invariant check fails with values identical to
GCC 14.3's, digit for digit:

| case | correct | GCC 14.3 | GCC 16.1 |
|---|---|---|---|
| Be | −14.3518804762 | −14.6890291293 | −14.6890291293 |
| O | −73.8041502333 | −74.5296496920 | −74.5296496920 |
| Ne | −126.6045249968 | −127.5114422775 | −127.5114422775 |
| N | −54.1053903978 | −55.0573624742 | −55.0573624742 |

Consequences:

- **Do not version-gate the workaround** — it is needed on both 14.3 and 16.1.
- **Beware the default compiler.** Homebrew's main `gcc` formula is now 16.1.0, so plain
  `gfortran` on this Mac *is* GCC 16; only CLAUDE.md's documented
  `-DCMAKE_Fortran_COMPILER=gfortran-14` pins 14. Both hit the bug, and the workaround covers
  any GNU compiler on arm64 Apple, but a developer's "default" build is not the documented one.
- **The two versions producing bit-identical wrong answers is worth weighing.** It is what you
  would expect from a persistent target-specific backend bug, but equally from source UB that
  both versions exploit the same way, so it does not settle the question — it only shows the
  behaviour is deterministic rather than a random codegen accident.

**Compiler versions in play (they were never harmonised, which hid this):** Linux `release/`
(the reference binary) used **plain `gfortran` = GCC 13.3.0**, Linux `build-rel/` and GitHub CI
use **gfortran-14**, and this Mac used **14.3 and now 16.1**. The committed reference `stdout`
files predate all of it and do not record what produced them — which is why the run banner now
stamps compiler and LAPACK version (`CMakeLists.txt` → `macros.in` → `molecule.main.foo`,
ignored by `scripts/test.py`).

**Left to do:**
- Decide the harmonisation: pick one reference compiler across Mac, Linux and CI. Worth doing
  with data — check whether GCC 16 reproduces the *same suite numbers* before switching, since
  otherwise the choice is between re-blessing references and staying put.
- Measure the runtime cost of `-O2` on this file (it is the ERI hot path). If negligible, stop.
- Consider a GCC bug report. The ingredients are unusually strong: bit-identical source, two
  platforms disagreeing, and a self-validating oracle (an SCF energy below the variational
  limit, virial 1.957 vs 2.000). Would need reducing to a minimal test case first.
- Re-test whether the *global* `-fno-schedule-insns` in `cmake/SetFortranFlags.cmake` is still
  needed now that the file is pinned — currently kept only because it was in the verified run.
- **Add a regression test**: for an s/p-only basis, `use_spherical_basis= T` and `F` must agree
  (the two bases are mathematically identical below d functions). That single invariant would
  have caught this immediately, on one machine, with no reference file.

## DONE: continuous integration (GitHub Actions, loose gate)

**Goal (user):** bring back automated CI so every push builds the ANTLR4 translator,
compiles `tonto`, and runs the test suite. (The old Travis badge was defunct.)

**Delivered** as `.github/workflows/ci.yml` — green as of `99dc3a1c` (2026-07-27):
- **Provider:** GitHub Actions (Travis's OSS offering is dead). Triggers on push/PR to
  `antlr4`, `master`, `release`.
- **Pipeline:** checkout `--recursive` → install gfortran-14 + JDK (for the translator's
  `javac`; the ANTLR jar auto-downloads via CMake) + BLAS/LAPACK/python3 → `cmake` +
  `cmake --build -j2` (bounded: one JVM per `.foo` is memory-heavy on a shared runner) →
  short suite via `scripts/suite_report.py`.
- **Gate:** the **loose** criterion (rel ≤ 0.2% OR last-digit ≤ 2). The agreement table is
  echoed to the run's Job Summary and uploaded as `tests.log`; a self-diagnosing `Diagnostics`
  step (`if: always`) reports toolchain/binary/one-raw-test on red runs. README carries the badge.
- **Two bugs the first green run flushed out** (both fixed): `scripts/test.py` resolved a
  *relative* `--test-directory`/`--basis-sets` after `chdir`ing into the temp run dir → doubled
  path → 100%-fail (fixed by absolutising up front, `036ecaec`); and the Kanghyun `keyword_echo`
  lines added un-reblessed stdout → 44/51 (dropped both echoes, `99dc3a1c`).
- **Not done (deliberate):** debug/release matrix and gfortran-version matrix — single release
  job for now; jar/parser caching not yet added (build is tolerable at ~20 min).

## Editor: improve vim highlighting of Foo and vim integration

**Goal (user):** improve the vim editing experience for `.foo` sources — better syntax
highlighting and tighter editor integration. The repo already ships some vim support
(`.vim/filetype.vim` maps `*.foo` and `macros` to a `foo` filetype; `scripts/fix_tags.pl` and
`scripts/cscope_setup` support ctags/cscope navigation — kept for exactly this reason).

**To investigate / do:**
- **Syntax file:** review/extend the `foo` syntax highlighting to cover the current language —
  reverse declarations (`var :: TYPE`), parameterized types (`VEC{T}`, `MAT{T}`…), pointer/
  allocatable suffixes (`*`, `@`), procedure headers with `:::` attributes (`PURE`,
  `ELEMENTAL`, `get_from(...)`, `selfless`), `KEY?` template placeholders, the constants
  (`TRUE`/`FALSE`/`ZERO`/`ONE`/`NULL`), and comments (`!`). Confirm whether a `syntax/foo.vim`
  exists and is up to date, or author one.
- **Indentation:** Foo uses 3-space indentation to mark scope (closed by `end`); an
  `indent/foo.vim` that follows this would help.
- **Navigation:** verify `scripts/cscope_setup` + `scripts/fix_tags.pl` still produce usable
  tags/cscope indexes for `foofiles/` and `runfiles/`, and document the workflow.
- **Integration niceties (optional):** a command/`makeprg` to translate the current `.foo`
  with `FooToFortran` and jump to errors; folding on scope; matchit for `... end` blocks.

## DONE: explicit `self` intent via self-modification analysis (plan B)

**Goal (user):** make `self`'s intent explicit in the `.foo` sources where it is
currently implicit. The first attempt used a blanket rule (subroutine → INOUT,
function → IN); it did **not** compile — read-only subroutines given INOUT reject a
const `self` from their (often inherited) callers, and some *functions actually
modify self* (memoisers, lazy readers), so `self :: IN` was rejected. Dylan had
assumed all functions are PURE, which several are not.

**Resolved with "Option 2" below** — a **self-modification analysis** in the
translator (`FooToFortran --add-self-intent`, parse-tree driven). Rule:
- subroutine → **INOUT** iff it modifies self, else **IN**;
- function → **IN** iff pure (does not modify self); a self-modifying function is
  left implicit and **flagged impure** (see `self_intent_analysis/impure_functions.tsv`).

"Modifies self" = direct write (`self%x = …`), a self-method call that transitively
modifies self (fixpoint, seeded with create/destroy/nullify), a call to a method whose
`self` is *declared* INOUT/OUT, or an input read into a self component
(`stdin.read(.label)`, `.SCF_DIIS.read_keywords`, `.atom(a).set_flag`).

**Applied + validated:** 135 `self :: IN|INOUT` decls (58 IN / 75 INOUT) across 47
foofiles, plus 2 genuinely-wrong hand-written `INOUT`→`IN` corrections on read-only
`MAT{REAL}` `_LAPACK` helpers. A clean **release** build compiles 0-error and the full
`ctest` suite is **121/124** — the same three deferred failures below, no regressions.

**Follow-on (deferred, Dylan's proposal):** mark the impure procs `IMPURE` in the
`.foo` and declare the rest `PURE`. Purity is compiler-enforced (a `pure` proc calling a
non-pure one is an error), so it self-validates. Impure = {modifies an arg or self} ∪
{does I/O} — so put/dump/show/read are impure regardless of self. The
`impure_functions.tsv` (modifies-self + OUT/INOUT-arg functions) is the seed list; add
I/O-call detection when tackling it.

## Deferred: prune dead and stale macros in `include/macros.in`

**Audited 2026-07-29.** Of **377** macros defined, **145 are never used in any `.foo` file**.
Fewer macros is better for maintenance (Dylan), but they are not all the same kind of thing and
should not be deleted with one sweep:

**(a) Genuinely dead — delete.** `TONTO_SET_STDERR` / `TONTO_SET_STDERR0` is the clearest case:
unused *and* it expands to `set_error_output_file_(tonto,X)` / `SYSTEM_set_error_output_file`,
a routine that **does not exist anywhere in `foofiles/`**. It would fail to compile the moment
anyone used it. Same family: `TONTO_CREATE`, `TONTO_DESTROY`, `PARALLEL_DO_START`,
`PARALLEL_DO_STRIDE`, `LOCK_PARALLEL_DO`, `UNLOCK_PARALLEL_DO`, `PARALLEL_VECTOR_SUM` — each
defined, each unused (their `…0` variants *are* used, inside `macros.in`).

**(b) Stale defaults — the real hazard.** Macros that look like the tunable default for
something but no longer drive anything, because the code sets its own value. Editing one has no
effect, which is a trap. Worse, at least one has *drifted*:

```
macros.in:629   ROBY_ZERO_ANGLE_CUTOFF   TOL(2)
roby.foo:176    .zero_angle_cutoff = TOL(2)*RADIAN_PER_DEGREE     <- not the same value
```

**These are safe to delete: the value has migrated into the type component's `DEFAULT(...)`
in `types.foo`** (Dylan — confirmed: `types.foo` carries 1103 `DEFAULT(...)` declarations).
The documentation is not lost by deleting the macro; it now lives on the component that owns
it, which is the better place. Checked one by one:

| macro | macro value | `DEFAULT` in `types.foo` | |
|---|---|---|---|
| `ROBY_OUTPUT_THETA_INFO` | `TRUE` | `TRUE` | matches |
| `ROBY_ANALYZE_ALL_ATOM_PAIRS` | `FALSE` | `FALSE` | matches |
| `QUADRATURE_ACCURACY` | `TOL(6)` | `TOL(6)` | matches |
| `QUADRATURE_MAXIT` | `10` | `10` | matches |
| `ISOSURFACE_ISO_VALUE` | `ONE` | `ONE` | matches |
| `TEXTFILE_MARGIN_WIDTH` | `0` | `0` | matches |
| `TEXTFILE_SPACING` | `2` | `2` | matches |
| `TEXTFILE_INT_WIDTH` | `8` | `8` | matches |
| `ISOSURFACE_TABLE_LENGTH/SPACING/EPS` | `30.0d0` / `0.02d0` / `TOL(9)` | `INTERPOLATOR_TABLE_*` | re-homed to the owning module |
| `TEXTFILE_COMMENT_CHARS` / `_QUOTE_CHARS` | `"!#"` / `"'"""` | `BUFFER_COMMENT_CHARS` / `BUFFER_QUOTE_CHARS` | re-homed |
| **`ROBY_ZERO_ANGLE_CUTOFF`** | `TOL(2)` | `TOL(2)*RADIAN_PER_DEGREE` | **DIVERGED** |
| **`PLOT_GRID_PLOT_FORMAT`** | `"gnuplot.contour"` | `" "` | **DIVERGED** |
| `DIIS_ERROR_TEMP_CUTOFF`, `FILE_BUFFER_LENGTH` | `TOL(2)`, `1024` | none found | check individually |

The two **DIVERGED** entries are the argument for doing this sooner rather than later: they are
not merely dead but *wrong*. Anyone "tidying up" by wiring the code to `ROBY_ZERO_ANGLE_CUTOFF`
would silently drop the degrees→radians conversion.

Remaining unchecked in this class: `ADAPTIVE_QUADRATURE_ACCURACY`, `QUADRATURE_EPS`, the other
`ISOSURFACE_*`/`PLOT_GRID_*`/`TEXTFILE_*` entries, `MULTI_T_ADP_TOL_0`,
`REAL_MAX_DECIMAL_PLACES`, `BASIS_LIBRARY_ENV_NAME`, `TONTO_REPOSITORY_BASIS_DIRECTORY` — same
method: find the component, compare its `DEFAULT` with the macro, delete if superseded.

**(c) Unused but arguably intentional API — leave alone.** Tonto is a library, and these are
language surface a future `.foo` could legitimately use: the kind/size families (`INT_1_SIZE`,
`REAL_16_SIZE`, …), the alternate scalar types (`INT_1/2/4/8`, `REAL_4/8/16`, `CPX_4/8/16`,
`CHR`, `BSTR`), `MAT6`/`MAT7`, and the physical-constant set (`AVOGADROS_NUMBER`, `BOHR_SI`,
`KCAL_PER_HARTREE`, …). Same reasoning as keeping the `set_width_automatically` fix.

**Caveat on the audit:** it counted uses in `foofiles/*.foo` and inside `macros.in` only. A
macro could in principle be referenced from another `include/` file or by the build, so confirm
before deleting any individual one.

## Deferred: `std_err` writes into the *input* file (hard-coded unit collision)

**Found 2026-07-29** while instrumenting `table_column.foo` to chase the zero(error) problem.
A few `std_err.show(...)` calls added for debugging did not appear in any error log — they were
**appended to the `stdin` file**, which corrupted the input while it was still being read and
killed the job with:

```
Error in TEXTFILE:read_line_bad_EOF ... unexpected end of file
File name = stdin   Line number = 35
```

The input file grew from 55 to 101 lines during the run. Reproducible.

**Mechanism — partly established, not fully pinned.** The three units are distinct
(`TEXTFILE_STDIN_UNIT` 5, `TEXTFILE_STDOUT_UNIT` 6, `TEXTFILE_STDERR_UNIT` 7, `include/macros.in`).
But `create_std_err` (`textfile.foo`) never *opens* anything — it allocates the object and
claims the hard-coded unit:

```fortran
std_err.name   = "stderr"
std_err.action = "write"
std_err.unit   = tonto.std_err_unit      ! = 7, hard-coded
```

Fortran does not pre-connect unit 7, and Tonto hands out units dynamically when it opens files,
so the likely story is that unit 7 already belonged to another open file (the input) and the
write simply went there. That has not been proved — worth confirming with an `inquire` on the
unit at the point of the write.

**Why it matters more than it looks:** a diagnostic channel that silently destroys the input is
a trap exactly when someone is debugging, and it fails in a way that looks like a *parse* error
in the user's input rather than an I/O bug. It cost real time here.

**Suggested fix:** have `create_std_err` claim its unit the same way every other file does
(via the allocator, checking `unit_used`) instead of assuming a fixed number, or open the file
properly. Note `stdout`/`stdin` share the same hard-coded pattern and may be latently exposed
to the same collision.

**Naming note:** the object was renamed `stderr` → `std_err` (matching the existing `std_time`,
`std_name`, `std_output` family) because the Unix name implied Unix behaviour it does not have.
The *file* it writes is still called `stderr`, so test directories and `IO` manifests are
unaffected. The CPP macro `TEXTFILE_STDERR_UNIT` was deliberately left alone: it is one of a
trio with `TEXTFILE_STDIN_UNIT`/`TEXTFILE_STDOUT_UNIT`, and renaming just one would look odd.
Renaming `stdin`/`stdout` likewise is ~12,500 call sites across 81 foofiles but — importantly —
**zero** test churn, since the file names are set separately. Deferred as cosmetic.

## Deferred: small numerical differences (longstanding) — drill down

Several tests differ from their references only by small numerical amounts — 3rd–4th
significant figure, or a last-digit wobble on a near-zero value. They pass the loose gate
(rel ≤ 0.2% OR last-digit ≤ 2), but some sit close enough to the boundary that a different
runner/CPU (BLAS / eigensolver ordering, FP reassociation) flips the verdict — this is the
**CI flake** seen on GitHub Actions (same binary, pass on one runner, fail on the next).
**Dylan wants to drill down** on each and fix the root cause, not merely tolerate them.

### Lower priority: `ylid` (rgbi) — vdW contact indices differ on macOS

**Status 2026-07-29:** fails on **macOS only** (3.85% max rel); **passes on Linux** against the
current reference, with both platforms on gfortran-16 and LAPACK 3.12.0.

**Where the difference lives:** splitting the diff at the *"Roby-Gould bond indices: VDW
interactions only"* boundary gives **14 of 17** differing hunks inside the vdW section and only
3 outside it (the largest of those being a Roby population, `C1 8.95` vs `8.96`). So the
van der Waals contacts — non-bonded pairs at 2.3–3.3 Å, enabled by `analyze_vdw_atom_pairs= T`
— carry most of the instability, as Dylan expected.

**Considered and NOT adopted: turning the vdW pairs off and re-blessing.** Two reasons:

1. **It would break Linux.** ylid currently *passes* there, so a re-bless would enshrine macOS
   numbers and flip the platform that is presently correct.
2. **It does not speed the test up** — the opposite. Measured interleaved, three rounds:
   vdW **ON** 45.9 / 46.7 / 46.3 s, **OFF** 59.5 / 59.5 / 59.1 s, i.e. ~13 s *slower* with
   fewer output lines (607 vs 660). Whatever `analyze_vdw_atom_pairs= F` does, it is not
   "skip work" — it selects a different and more expensive set of pairs. That oddity is worth
   understanding on its own; a flag that costs time when disabled is a bug smell.

It would also drop coverage of the vdW code path, and the 3 non-vdW hunks would likely remain.

**Also ruled out:** tightening the guess-SCF convergence. Settings from `convergence= 1e-3`
(default) down to `1e-6` leave both the runtime (~46 s throughout) and the disagreement
essentially unchanged (108 → 106 differing lines); `1e-8` makes the atomic SCF fail to converge
outright, so `make_ANOs` DIEs and the job aborts — which is the DIE working as intended, but it
also produced a *shorter* output that briefly looked like a speed-up and a perfect match. Beware
that trap when timing truncated runs.

**Open:** where the 46 s actually goes (convergence is not the lever), and why the vdW indices
are the platform-sensitive part.

### PRIORITY, NOT STARTED: NaN and negative ESDs from the least-squares variance-covariance matrix

**This is the live thread — pick it up here.** Two impossible esd values are confirmed by
instrumentation, in two different tests. An esd is a square root: it can be neither negative
nor NaN. Per Dylan the source is the **least squares — the variance-covariance matrix is not
right**, either a genuine error in its construction or UB.

**Evidence (both from probes inserted in `TABLE_COLUMN:set_values_and_errors`, since removed):**

| test | finding |
|---|---|
| `short/urea_lamaGOET_grown_CIF` | ADP U13/U23 columns: `e_neg = 2`, `e_zero = 2` of 5 rows — two **negative** esds |
| `long/urea_rhf_STO-3G_HAR` | one ADP column: **`e_nan = 1`**, with `prec_out = 5` (i.e. the column precision was normal — the *data* is bad, not the formatting) |

The `e_nan` probe result also killed the competing explanation that the column precision was
simply small (`max_dp=1` would give `dp=2` innocently); `prec_out` was 5 in every column.

**What has been fixed — formatting only, NOT the numbers.** `REAL:get_dp_de_le` now guards with
`NOT (abs_error>ZERO)`, which catches zero *and* NaN in one test (every comparison against NaN
is false), and takes `abs()` for the negative case. Previously these reached `log10()`, which is
undefined for both, and under `-ffast-math` rendered as 6 decimal places on Linux/x86 and 2 on
macOS/arm64 **from identical input** — the whole of the longstanding "zero(error) problem".
`TABLE_COLUMN:set_values_and_errors` now emits a `WARN` when a column contains a NaN or negative
esd, deliberately rather than silently printing `(0)`, because a NaN esd rendered as `(0)` is a
wrong number presented as a confident one.

**So the output is now well-defined and platform-independent; the esds are still wrong.**

**Open question — the warning is debug-only.** `WARN`/`WARN_IF` are gated on
`USE_PRECONDITIONS` (`macros.in:281`), which release builds do **not** define (only
`USE_ERROR_MANAGEMENT`, which gates `DIE`). So the new NaN/negative-esd warning **compiles to
nothing in a release build**: a production run silently prints `(0)` for a NaN esd. That is
defensible for a programmer precondition, but this is a statement about the **validity of
numbers written into a CIF** — possibly a published one. Options if that is judged wrong:
promote it to something always-on (not `WARN`), which changes output and forces a re-bless; or
leave it debug-only and rely on the upstream fix. Decision deferred, deliberately.

**THE PATH IS NOW MAPPED — start here.** Traced 2026-07-29 (static reading, no probe yet):

```
least squares  ->  .xray_data.covariance_mx            (MAT{REAL}, the v-cov matrix)
                        |
crystal.foo:8231        |  per-atom diagonal block, transformed:
                        |  covariance_mx(af:al,af:al).back_transform_to(C(af:al,af:al),T(1:n,1:n))
                        v
crystal.foo:8236   C.put_diagonal_to(esd)               <- esd = DIAGONAL of the transformed v-cov
                        |
crystal.foo:8038   atom.put_CIF_ADP2(...,esd)
vec{atom}.foo:1002 put_ADP2_errors_to(dU,fac,esd)       <- pure copy: dU(a,k) = esd(k+3+…)
                        |                                   then dU = fac*fac*dU
                        v
                   TABLE_COLUMN:set_values_and_errors(r,e)  <- where e_nan / e_neg were measured
```

**Note what `esd` actually is: the diagonal of a covariance matrix.** There is no `sqrt` in this
path — `put_diagonal_to` takes the diagonal straight out of `C`. So:

- a **negative** esd means the transformed covariance diagonal is negative, i.e. `C` is **not
  positive semi-definite** — either `covariance_mx` itself is not, or `back_transform_to`
  is losing it. This is Dylan's diagnosis, structurally confirmed.
- a **NaN** esd means a NaN is already in `covariance_mx`, or is produced by the transform.

`T` is built from `.unit_cell.inverse_mx` and the reciprocal matrices via
`GAUSSIAN_DATA:symmetric_tensor_{2,3,4}_product_mx` (`crystal.foo:8208-8221`); it is 34x34,
zeroed, with only the used sub-blocks filled, so an atom whose `no_of_pADPs` reaches into an
unfilled region would transform through zero rows — that yields zeros, not NaN, but is worth
checking while there.

**The next probe (one run localises it):** print, per atom block, the counts of NaN and negative
entries in `covariance_mx(af:al,af:al)` **before** the transform and in `C(af:al,af:al)`
**after** it, in `make_CIF_esds` (`crystal.foo:8179`). That says immediately whether the defect
is inherited from the least squares or manufactured by `back_transform_to`. Use `stdout` (NOT
`std_err` — see the unit-collision item), and the NaN test `x/=x` worked under the current
release flags.

**Then:** if it is inherited, chase `covariance_mx` back into the least-squares/normal-equations
inversion; a debug build with `-fcheck=all -ffpe-trap=invalid,zero,overflow` should trap where
the NaN is *created* rather than where it is printed. Remember `-ffast-math` is on in release,
so NaN behaviour there is not dependable.

**Older notes on where to look (superseded by the map above):**

1. Is the NaN/negative already present in the covariance matrix **diagonal**, or introduced by
   the transformation into the ADP basis? Print the diagonal before and after.
2. If it is in the diagonal — is it a `sqrt` of a negative, a `0/0`, or an uninitialised
   element? A debug build with `-fcheck=all -ffpe-trap=invalid,zero,overflow` should trap at the
   point of creation rather than at the point of printing.
3. Note `-ffast-math` is in force in release builds, so NaN-producing operations are *not*
   guaranteed to behave predictably; the debug build is the right place to chase this.

**Caveat on the NaN test itself:** `e/=e` is the standard NaN check but `-ffast-math` permits
the compiler to assume no NaNs and fold it away. It demonstrably *worked* with the current flags
(the probe found the NaN), but do not rely on it silently — verify it still detects when flags
change. The guard in `get_dp_de_le` uses `NOT (abs_error>ZERO)` partly for this reason.

### Superseded detail: negative ESDs (kept for the record)

**Found 2026-07-29** while tracking down `urea_lamaGOET_grown_CIF`. Instrumenting the ADP
columns showed that, of the five atoms, **two carry tiny *negative* esds** in the U13/U23
columns (`e_neg = 2`, `e_tiny = 2`, alongside two that are exactly zero):

```
DBG prec ....... 5      DBG e_neg ...... 2      DBG e_zero ..... 2
```

An esd is a square root and cannot be negative. **Dylan's reading: this comes from the least
squares — the variance-covariance matrix is not right, either a genuine error in its
construction or undefined behaviour.** Noise pushing a diagonal element slightly below zero
would do it, but that should be understood rather than clamped.

The *formatting* consequence has been fixed (`REAL:get_dp_de_le` now takes `abs(error)` before
`log10`, which was previously invalid for a negative argument and gave platform-dependent
decimal counts — 6 on Linux/x86, 2 on macOS/arm64). **That fix makes the output well-defined;
it does not make the esds correct.** The underlying negative variance is still there and is the
thing to chase.

Where to start: the ADP esds reach the CIF writer via `put_ADP2_errors_to`, which scales values
taken from `.xray_data.covariance_mx`. Worth checking whether the negatives are present in the
covariance matrix diagonal itself or are introduced by the transformation into the ADP basis,
and whether a debug build with `-fcheck=all`/`-ffpe-trap` catches anything in that path.

Examples so far:

| Test | Difference | Likely cause |
|------|-----------|--------------|
| `urea_ccsd_pob-TZVP_Salvador_properties` | Salvador charges/dipoles ~0.5% (`0.1984`→`0.1974`, `-0.3959`→`-0.3956`), 3rd–4th sig-fig | grid integration / partition numerics; longstanding, pre-ANTLR |
| `h2o_rhf_cc-pVDZ_tdhf` | TDHF response, rel ~0.12% — just under the 0.2% gate (ulp already ~10) | time-dependent HF response; eigensolver / BLAS ordering across runners |
| `nh3_rhf_DZP_HAR` | a near-zero value: ~10% *relative* but ~1 ulp — passes only via the last-digit bound | relative metric amplified near zero (cf. the `ylid` case) |

**Stopgap in place (CI stability):** `scripts/suite_report.py` carries a documented
`KNOWN_MARGINAL` table that widens the loose bound for just these tests (`tdhf`: rel ≤ 0.5%;
`nh3_rhf_DZP_HAR`: last-digit ≤ 4) so the badge stops flickering, while the strict 0.2% / 2 gate
stays for every other test — and the report prints a footnote naming which tests were relaxed.
It is a **workaround, not a fix**: the goal is to *remove* entries from that table by
understanding each discrepancy. (Salvador is not in the table; its ~0.5% is accepted with the
reference pinned to the produced values.)

NOTE (Salvador): verify this is NOT the moments-staleness knock-on from setting `.atomic_moments_made`
(the flag now suppresses moment re-making that release always did). If a targeted
`.atomic_moments_made = FALSE` reset after SCF convergence restores the reference values, it
IS the knock-on and should be fixed rather than accepted. See memory `debug-ensure-vs-release`.

## DONE (release): the 3 remaining test-suite failures (milestone 3)

**Resolved on release — verified 2026-07-17: a `gfortran-14` release build is 124/124 (`ctest`
exit 0), up from 121/124.** All three former failures now pass, fixed at the source / in the
references by Dylan (commits `b3b50dd2` "Fixed no. of doubles test error", `d9dffb3f`,
`dee5cac9` "Corrected Salvador test", `50988e87` "All short & long tests passing on laptop"):

1. **`cyclazine_rhf_cc-pVDZ_tddft_state_selection`** — was a single-line `No. of doubles`
   diff (`24355` ref vs `22797`). **Not** an `-O0` boundary artifact after all: it was a
   real **evaluation-order bug in the source**. `foofiles/td_data.foo` computed
   `n = .no_of_doubles` *after* printing the doubles-window block; commit `b3b50dd2` moved the
   assignment *before* the block, so the printed count is now correct and stable. (Also fixed
   two typos in adjacent `stdout.text` lines.)
2. **`gly_ala_fragHAR_rhf_STO-3G`** — table column-width / alignment shift; reference updated
   (`50988e87`). Passes (73 s).
3. **`urea_ccsd_pob-TZVP_Salvador_properties`** — the longstanding Salvador grid/partition
   numeric difference; the reference was updated to the release-produced values (`dee5cac9`),
   so the release build now matches exactly. **NB:** this "resolves" it only for `-O3`; the
   ~0.5% `-O0` difference is unchanged and now shows up as a **debug-only** failure — it has
   moved into the debug section below, not disappeared. The `atomic_moments_made` knock-on
   question (see the Salvador section above) is still unverified.

Two other cases seen only under the *strict* (exact) sweep also loose-pass and are benign:
`h2o_rhf_cc-pVDZ_tdhf` (one TDHF state differs in the last digits) and
`cyclazine_rhf_cc-pVDZ_VMO_canonicalization` (~1e-4; original archives lost, regenerated).
A threshold-driven "loose pass" gate (candidate for CI, above) absorbs all of these.

What remains for milestone 3 is therefore **not** the release suite (green) but the **debug**
suite (119/124 — next section) and wiring the release gate into CI.

## Deferred: debug-build (`-O0`) test failures — floating-point boundary artifacts

> **FIXED & COMMITTED — `process_CSD_cif` (#113) fragment-offset `int()` flip (2026-07-26).**
> The worst debug failure — the `Fragment offset` `int()` boundary flip (`0.999.../1.000...` truncating
> to different integers under `-O0` vs `-O3`, cascading a whole lattice-vector shift, 189% rel) — is
> **fixed at the source**. Root cause: `int(mean_column_vector)` truncates toward zero with its knife-edge
> exactly on the integers, where a fragment centre can land. Fix: a **toward-zero nudge**,
> `int(pos*(ONE-TOL(8)))`, at all **6** offset sites (`cluster.foo` 132/163/529, `crystal.foo`
> 979/10329/10363). This shrinks the centre by `1e-8` (≫ the ~1e-14 `-O0/-O3` reassociation gap,
> ≪ the 1e-6 coordinate resolution) so both optimisation levels resolve the boundary to the **same**
> integer, while every non-boundary fragment keeps its exact `int()` value.
>
> Rejected alternative: `nint()` (round-to-nearest). It also makes `-O0`==`-O3` but changes the
> *recentering convention* for **every** fragment (e.g. mean 0.616: `int→0` but `nint→1`), churning
> ~624 lines across many cluster tests. The nudge is surgical.
>
> **Verified:** both trees rebuilt 0-error; on `process_CSD_cif` **release output == debug output**
> (0 substantive diff — the `-O0/-O3` disagreement is gone) and release is deterministic run-to-run.
> The `stdout` reference was re-blessed from the release build: it changed only by **one boundary
> fragment** (offset `0 1 0`→`0 0 0`, a pure lattice relabelling; `iodos.cxc` is unchanged — the offset
> cancels in absolute coords). No other test moves. Committed (source `cluster.foo`, `crystal.foo`;
> reference `process_CSD_cif/stdout`).
>
> This fix does **not** address the other 4 debug failures (47, 64, 87, 91) — the small-FP / esd-token /
> convergence-divergence cases in the table below, still open.

**Context (re-measured 2026-07-17).** A clean **debug** build (`gfortran-14`, `-O0 -g`, ENSURE
preconditions live) compiles 0-error and runs the full suite at **119/124** (up from 116/124
on 2026-07-15 — the same-day release is 124/124). Every remaining failure was checked against
the **release** binary (`-O3`), and **release reproduces the reference exactly** for all of
them. So **none is a translator bug or a crash** — the debug build ran every job to completion
with no ENSURE aborts. The *only* variable is optimisation level: `-O3` FP contraction /
reassociation produces sub-ulp numeric differences that, at boundary cases, **flip a discrete
decision** in the source. Debug's real job — surfacing crashes and precondition violations —
passed clean.

**Changes since 2026-07-15 (net +3):** two ADP-label cases (`L_alanine` #65, `YLID` #69) were
cleared by Dylan adding `show_IAM_output=FALSE` / `show_IAM_results=TRUE` to their `stdin` —
this suppresses the per-cycle ADP tables where the near-equal `Uxx`/`Uyy` label flips, while
keeping the final refined values compared (exactly the "suppress the line, keep the meaning"
approach below). `cyclazine` #5 was fixed at the source (see the DONE section above). Against
that, `urea…Salvador` #47 **entered** the debug failure list: its reference was updated to the
`-O3` values, so `-O0` now differs from the reference by the longstanding ~0.5% grid amount.
Net: 8 − 4 (65, 69, 5, 72) + 1 (47 gained) = 5 failures; #113 then fixed at the source
(nudge, above) = **4 failures** (47, 64, 87, 91).

**Proven mechanism** (was `#113`, now fixed — kept as the canonical illustration of the class).
`foofiles/cluster.foo:132` — `.fragment_offset = int(crystal.fragment_geometry.mean_column_vector)`.
`int()` truncates the fragment-centre mean toward zero; when that mean sits on a unit-cell boundary,
`-O3` yields e.g. `1.0000001 → 1` while `-O0` yields `0.9999999 → 0`. Both are crystallographically
valid (differ by a lattice vector) but print differently. The site now carries the `*(ONE-TOL(8))`
nudge (above), closing this instance; the remaining ADP-label and count cases below are the same
class (selecting/counting among near-equal or near-threshold values).

**Current debug failures (4; #113 fixed 2026-07-26, others measured 2026-07-17):**

| # | Test (category) | Substantive diff (ref → debug) | Class |
|---|-----------------|--------------------------------|-------|
| 64 | `ylid` (rgbi) | last bond-analysis columns drift ±0.1 (e.g. `74.02`→`73.90`); worst is `0.04`→`0.05` = 20% *relative* on a near-zero value | FP noise amplified by relative metric near zero |
| 87 | `urea_rhf_DZP_consistent-cluster-charge_HAF` (long) | 1-ulp last digit (`-349.2012`→`-349.2013`) + one column 1 char wider | last-digit rounding + auto-width threshold |
| 91 | `yq28_H_U_iso_IAM_refinement` (long) | identity matrix width `1.0000`→`1.000` (numbers identical); **also** two extra lines `Warning … crystal data already defined!` / `xray_data is already defined` not in the reference — release does not print them, so investigate before assuming FP (may be an ENSURE/precondition path live only in debug, or a junk-filter gap) | auto-width threshold + unexplained debug-only warnings |
| 47 | `urea_ccsd_pob-TZVP_Salvador_properties` (short/long) | Salvador charges `0.1984`→`0.1974`, `-0.3959`→`-0.3956`, dipoles ~3rd–4th sig-fig (~0.5%) | longstanding grid/partition numerics (see Salvador section); reference now pinned to `-O3` |

**Cleared since 2026-07-15** (kept for the record): #65 `L_alanine` and #69 `YLID` ADP-label
flips (fixed via `show_IAM_output=FALSE` in their `stdin`); #5 `cyclazine` No.-of-doubles
(source fix `b3b50dd2`); #72 `gly_ala_fragHAR` (now passes debug too); #113 `process_CSD_cif`
fragment-offset `int()` flip (source fix — `*(ONE-TOL(8))` nudge, 2026-07-26; see the block above).

**Goal (Dylan): make the *debug* tests pass**, probably by **suppressing the offending
output line(s)** in the comparison — but not so much that the test loses meaning. Notes on the
options, to think through:

- **Targeted output suppression (Dylan's lead).** Each failure is one or a few identifiable
  lines: the `Fragment offset` line, the ADP component *label* token, the `No. of doubles`
  count, the auto-width columns. Adding these to `prefixes_to_ignore` (or a per-test ignore
  list) in `scripts/test.py` makes debug green while keeping the numeric substance compared.
  Risk: suppressing a *label* or a *count* removes a genuinely meaningful field — prefer
  suppressing only the specific line, per-test, not the whole table.
- **`test.py` near-zero `abs_tol`.** Only helps the pure-numeric near-zero case (`ylid`
  `0.04` vs `0.05`). Does **not** fix the discrete label/offset/count flips (those are text).
- **Source hardening (higher value, more invasive).** Replace knife-edge `int()` /
  component-selection / auto-width thresholds with a small-epsilon-tolerant form so `-O0` and
  `-O3` agree at boundaries. This has real **portability** value — a boundary that flips
  between `-O0` and `-O3` could also flip between compilers/platforms even in *release* — but
  it edits hand-written scientific `.foo` and must be done per-site (start `cluster.foo:132`).

To reproduce: clean `gfortran-14` debug build (`-DCMAKE_BUILD_TYPE=debug`), `ctest`, then
`diff tests/<suite>/<test>/stdout tests/<suite>/<test>/stdout.bad` for each failure (the loose
harness writes `stdout.bad` on a fail). The five listed above are all that remain as of
2026-07-17; the raw diffs from the original 2026-07-15 run lived in that session's scratchpad
(`debug_analysis/`) and are not preserved across sessions.

## Column-0 `#ifdef`/`#endif` inside a program body — benign parser diagnostic

**Date:** 2026-07-28. A release build prints, at ~98%:

```
[ 98%] Generating run_mpi_test_complete.F90
line 234:0 extraneous input '<EOF>' expecting {END, USE, 'interface', ... }
```

**Cause.** `runfiles/run_mpi_test_complete.foo` (and `run_mpi_test.foo`) wrap their executable
body in a **column-0** `#ifdef MPI ... #endif` that sits between the indented `program` body and
the final `end program`. Foo block scoping is **indentation-based**; the grammar passes
`#include`/macros through pre-CPP but does **not** model column-0 `#if*`/`#endif` directives, so
the indent/dedent tracking can't cleanly match the closing `end program` and the parser reports a
spurious EOF expectation at the true end of file.

**Impact — benign.** ANTLR error-recovers and still emits valid Fortran (`run_mpi_test_complete.F90`
ends correctly and the executable links). Only these two MPI test harnesses use the pattern; no
`foofiles/` module, `tonto`/`hart`, or `ctest` is affected. Not a regression — the files predate
current work.

**Fix (deferred).** Teach the Foo lexer to treat CPP directive lines
(`#if`/`#ifdef`/`#ifndef`/`#else`/`#elif`/`#endif`) as passthrough that does not perturb
indentation/scoping. Low priority. (Separately, 2026-07-28: `run_mpi_test_complete.foo` had its
variable declarations moved *inside* the `#ifdef MPI` so they don't trigger unused-variable
warnings in a non-MPI debug build; this does not remove the parser diagnostic above.)
