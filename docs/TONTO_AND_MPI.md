# Building and validating Tonto with MPI

Status: **milestone 4 in progress** (started 2026-08-01). This document records how to build an
MPI Tonto, the traps that are easy to fall into, the defects found the first time it was done,
and the numeric characterisation against a serial build.

Before this, **no MPI build had ever been configured** — not in CI, not in `install-all.sh`, and
not in any local build tree. Everything below is first-contact knowledge.

## 1. The toolchain trap: your MPI must match your Fortran compiler

`foofiles/parallel.foo` does `USE mpi`, and `include/macros.in` needs `MPI_ADDRESS_KIND` from
that same module. Fortran `.mod` files are **compiler-version specific**, so an MPI package built
against a different gcc simply cannot be used:

```
Fatal Error: Cannot read module file '/opt/homebrew/.../mpi.mod' opened at (1),
because it was created by a different version of GNU Fortran
```

This is not fixable with `OMPI_FC`: that changes which compiler the *wrapper* invokes, not the
compiler that built the shipped modules.

Configure now detects this and stops immediately with an actionable message, rather than failing
hundreds of files into the build:

```
-- Performing Test TONTO_MPI_MODULE_USABLE - Failed
CMake Error: The MPI installation provides Fortran modules this compiler cannot read.
    Fortran compiler : GNU 14.3.0
    MPI Fortran      : /opt/homebrew/bin/mpif90
```

**If your packaged MPI does not match, build one that does.** It coexists happily with the
system copy; select it with `PATH`:

```sh
curl -O https://download.open-mpi.org/release/open-mpi/v5.0/openmpi-5.0.9.tar.bz2
tar xf openmpi-5.0.9.tar.bz2 && cd openmpi-5.0.9
./configure --prefix=$HOME/opt/openmpi-gf14 \
            FC=gfortran-14 CC=clang CXX=clang++ --disable-mpi-cxx \
            --with-libevent=$(brew --prefix libevent) \
            --with-hwloc=$(brew --prefix hwloc) --with-pmix=internal
make -j10 && make install
```

On macOS/Homebrew the three `--with-*` options are required: the stock configure fails in PRRTE
with *"Either libevent or libev support is required, but neither was found"*.

## 2. Building

```sh
OMPI=$HOME/opt/openmpi-gf14
cmake -S . -B build-mpi \
      -DCMAKE_Fortran_COMPILER=$OMPI/bin/mpifort \
      -DCMAKE_C_COMPILER=$OMPI/bin/mpicc \
      -DMPI=1 -DCMAKE_BUILD_TYPE=release
cmake --build build-mpi -- -j5
```

Confirm you actually got an MPI binary — the run banner's `Compiler:` line must show the
expected gfortran, and:

```sh
otool -L build-mpi/tonto | grep mpi     # ldd on Linux
```

Notes on the recipe:

- **`-DMPI=1` is now a hard requirement.** It used to report a miss as a `STATUS` line and build
  a *serial* binary that looked like it had honoured the flag.
- `-DCMAKE_CXX_COMPILER=mpicxx` was in the old README recipe and is **ignored** — `project()`
  enables `Fortran C` only.
- `-DNO_ERROR_MANAGEMENT` was documented but is a **no-op**; the symbol exists nowhere in the
  build system. Removed from the docs rather than implemented.
- Use `-DCMAKE_BUILD_TYPE=release` for any comparison against the reference outputs: they were
  blessed at `release`, whereas `fast` adds `-faggressive-loop-optimizations -fstrict-aliasing`.

## 3. Testing

```sh
# rank-invariance smoke test -- run this FIRST, before believing any suite numbers
ctest --test-dir build-mpi -L mpi
sh scripts/check_mpi_pi.sh build-mpi/run_mpi_pi $OMPI/bin/mpirun 1 2 4

# the reference suite, under the launcher
python3 scripts/suite_report.py --program build-mpi/tonto --suites short \
        --mpi --mpi-ranks 4 --mpi-launcher $OMPI/bin/mpirun
```

`scripts/test.py` and `scripts/suite_report.py` both gained `--mpi-ranks` and `--mpi-launcher`.
Previously the rank count was hard-coded to 4 in `test.py` and `suite_report.py` had no `--mpi`
option at all, so `make report` against an MPI build silently ran everything single-rank.

**`scripts/check_mpi_pi.sh`** is the invariant test: `run_mpi_pi` integrates π with one
`parallel do` and one `PARALLEL_SUM`, so it exercises the whole macro surface every parallel
routine depends on, against an answer known in advance that must not change with the rank count.
Unlike the reference suite it needs no stored output, so it cannot be silently blessed. It also
separates the two failure modes the reference suite cannot: wrong at *every* rank count means the
reduction or the integration is broken; right at `-n 1` but wrong at `-n 2`/`-n 4` means partial
sums are not being combined.

## 3a. CI

`.github/workflows/ci-mpi.yml` — **CI (Linux-MPI)**, the first CI coverage the MPI build has ever
had.

**Trigger it by hand** from Actions → *CI (Linux-MPI)* → *Run workflow*. It also runs weekly
(Mondays 05:17 UTC) and on pushes that touch MPI-relevant paths (`parallel.foo`, `system.foo`,
`macros.in`, `run_mpi_*.foo`, the test scripts, or the workflow itself). Deliberately **not** on
every push: Tonto does `USE mpi`, so the MPI must be built by the same compiler as Tonto, and
Ubuntu's packaged Open MPI is built against the distro default (gcc-13 on 24.04) — so the
workflow has to build Open MPI from source. That is cached on `(Open MPI version, gfortran
version)` — the pair that `.mod` compatibility actually depends on — so a cold cache costs
~8-10 min and a warm one seconds. Same pattern `ci-wsl.yml` uses for its expensive job.

**The workflow compiler is `gfortran-16` as of 2026-08-26** (`FC_VERSION` in `ci-mpi.yml`, the
single place it is set; Ubuntu 24.04 stops at gcc-14, so it comes from
`ppa:ubuntu-toolchain-r/test`). **The whole project followed on 2026-08-27** — every workflow,
the release tarballs and the build documentation — accepting the loss of `-fcheck=bounds` in
debug builds; see `CLAUDE.md` §4.

The measurements recorded further down this page were taken under **gfortran-14** and are left
as measured. Two notes on re-measuring them, agreed 2026-08-27:

- **The serial comparison is cheap and worth having.** `ci-full-suite.yml` takes an
  `fc_version` dispatch input, so dispatching it at `14` and at `16` gives a like-for-like pair
  on identical code, references and hardware — the only honest way to attribute drift to the
  compiler. It has diagnostic value now, not just after GCC 16 releases: milestone 7 is being
  chased *on* 16 while its baseline numbers come from 14, which leaves compiler and code varying
  together.
- **A single MPI run must not be used for this.** Finding 7 measured five `ci-mpi.yml` runs on
  identical code at ERROR 1, 1, 1, 1 and **11**. Any 16-vs-14 MPI claim needs repetition on both
  sides first. Re-running §6's four-build sweep is deferred until GCC 16 is released, since the
  numbers would otherwise want taking twice.

**What gates it:** the π rank-invariance check (`check_mpi_pi.sh` at 1/2/4 ranks). It needs no
stored reference, so it cannot be silently blessed, and all four dead reductions found in
milestone 4 would have failed it.

**What does not gate it:** the short suite at `-n 2`, which is informational until the defect
register below is clear — five rows remain, so a gating suite would be permanently red. That is
the same reasoning `ci-debug.yml` applies to the four longstanding `-O0` failures. `-n 2` rather
than `-n 4` because the runner has 4 vCPUs.

**Every check asserts on an artefact, not on process state** — that `mpifort` really wraps
gfortran-14, that a `use mpi` program compiles *and runs* under the matching launcher, that
`ldd build-mpi/tonto` shows `libmpi`. That convention is deliberate: a day of milestone-4
debugging was lost to checks that could not see what they were checking — a stale binary reported
as fresh, and a `pgrep` matching its own shell.

## 4. What an MPI build changes, besides adding ranks

Worth knowing before interpreting any numeric difference:

- **`PURE` and `ELEMENTAL` are `#undef`ed across the entire codebase** (`include/macros.in:256`).
  MPI calls are impure and some `PURE` routines contain `PARALLEL_SUM`, so the purity contract
  cannot hold. This is a large codegen change — it costs common-subexpression elimination,
  loop-invariant hoisting and `elemental` vectorisation everywhere — and it is why an MPI build
  differs from serial **even at one rank**. Only routines that transitively reach a `PARALLEL_*`
  macro actually need it; narrowing that set is recorded in `DEFERRED.md`.
- The `PARALLEL` type layout differs (`types.foo:376` has `#ifdef MPI` members).
- Work is distributed **cyclically**: rank *r* takes iterations *r, r+P, r+2P, …*
  (`parallel.foo:243`). Partial sums are therefore rank-partitioned, and reduction order depends
  on the rank count.

## 5. Defects found on first contact

Full detail, with evidence, in `DEFERRED.md` under *"MPI: defects found during
milestone 4"*. Summary:

**Fixed** (these produced wrong answers, not drift, and would have made the characterisation
meaningless):

| Where | What |
|---|---|
| `molecule.grid.foo` ×4 | ESP/property-grid reductions written *inside* their own `parallel do`, where `WORK_IS_SHARED` is always false — dead code, so each rank kept only its `1/n_ranks` share. One had no reduction at all. |
| `parallel.foo` | `PARALLEL_SYMMETRIC_SUM_23` sized its triangle buffer from `dim1` instead of `dim2`: a heap overflow on every call, plus an `ENSURE` testing the wrong dimensions. |
| `molecule.fock.foo` ×3 | CIS/TDHF: `r_CIS_S1_AV` reduced nothing at all; `r_CIS_S0_AV` and `u_CIS_AV` never reduced `K`. |
| `system.foo` | `set_per_rank_IO_allowed` assigned `.keyword_echo`, not `.per_rank_IO_allowed` (2026-08-03). The flag could therefore **never be set**, so the escape hatch in `SYSTEM:IO_is_allowed` was unreachable dead code, every "let each rank do its own I/O" call site in `molecule.scf.foo` was a silent no-op — non-master ranks dropped their writes — and those call sites were instead toggling an unrelated flag. Longstanding: `set_parallel_IO_allowed` had the identical body before the rename. |
| `molecule.scf.foo` | `fragment_SCF_para` switched per-rank I/O mode on and then off again *inside* the fragment loop body, so the bookkeeping broadcasts resumed while the ranks were on different fragments. It must be set once, outside the loop, on every rank — including the >2-rank master, which schedules and `cycle`s without entering the body. |
| `molecule.put.foo` | `put_atom_group_mols` was collective, and branched on state that is *deliberately* per-rank after a fragment loop (`if (.becke_grid.allocated) …` — master issued 42 broadcasts there, rank 1 zero). Now non-collective: per-rank mode on, master writes alone, caller's mode restored. |

All the fixes are no-ops in a serial build, and this was verified: the short suite gives
bit-identical results before and after.

**Result of the last three:** `hart` now runs **fragHAR under MPI**. `mpirun -n 2 hart …
--group-charges '{ 1 -1 }'` on gly-L-ala exits 0 and reproduces the serial reference digit for
digit — R(F) 0.032423, GoF 3.353475 — with the ranks in exact lockstep for 2,421,451
broadcasts. See `docs/RUNNING_HART.md`, milestone H1.

**The general rule these establish:** after a per-rank region the ranks' object graphs are
deliberately different, and any later shared-mode code that branches on allocation status,
extent or convergence flags of that data will desync. Either resynchronise the state or keep
the later code non-collective. Because TEXTFILE bookkeeping is collective, *printing slightly
more on one rank is itself a collective mismatch* — and it surfaces later, somewhere innocent.
Pitfall 8 in `docs/TONTO_DEVELOPER_INFO.md` §1a, with the trace-based recipe that found it.

**Not yet fixed** — a latent collective-inside-a-master-guard deadlock in `SYSTEM:initialize`, a
commented-out `MPI_ABORT` (so one rank dying hangs the job), HAR writing the same file from every
rank, an out-of-bounds read in the `fragment_SCF_para` RMA work queue, and a QTAIM decomposition
that breaks at `nprocs == 1`. See `DEFERRED.md`.

### The root cause, and the agreed fix

Most of the above is one mistake repeated. `FooToFortran` emits `LOCK_PARALLEL_DO` as the *first
statement inside* a `parallel do`, and `work_is_shared` is false while that lock is held — so any
`PARALLEL_*` macro written in the loop body is a **silent no-op**. The intent ("MPI on the
outside", no interior collectives) is standard and correct; the enforcement is invisible, so
correctness depends on the programmer tracking the dynamic call sequence by hand.

Agreed fix (milestone 6, urgent, immediately after this characterisation): add a
`parallel do … reduce(x)` clause lowered by the translator, so the reduction is emitted in the
one correct place and cannot be misplaced; abort on a suppressed reduction under
`USE_PRECONDITIONS`; depth-count the lock; and lint for the pattern in the translator.

## 6. Numeric characterisation

Method: four builds, all gfortran-14, all `release`, run against the **same stored references**,
attributing drift by differencing the agreement tables. Nothing is re-blessed.

|            | `-Ofast` (shipped)  | `-O2 -fno-fast-math` (control) |
|------------|---------------------|--------------------------------|
| **serial** | `build-serial-fast` | `build-serial-o2`              |
| **MPI**    | `build-mpi-fast`    | `build-mpi-o2`                 |

The `-O2 -fno-fast-math` pair exists because `release` uses `-Ofast`, which licenses the compiler
to reassociate floating-point sums; without the control you cannot tell compiler reassociation
from MPI reduction order.

Attribution:

- `serial-O2` vs `serial-Ofast` → compiler reassociation alone
- `MPI -n 1` vs serial at the same `-O` → MPI-build effects (`PURE` stripped, MPI code paths)
- `MPI -n 2` vs `-n 4` → genuine rank-count-dependent reduction order
- anything left over → candidate real bug

### Baseline caveat

The short suite on this macOS box is **50/51**, not the 51/51 recorded for Linux CI:
`urea_ccsd_pob-TZVP_Salvador_properties` fails the loose gate at 2.99 % relative against the
Linux-blessed reference. This reproduces exactly on a pre-existing binary, so it is a
platform difference unrelated to MPI — but that test cannot contribute to the comparison.

### Results (short suite, 51 tests, macOS arm64, gfortran 14.3.0, Open MPI 5.0.9)

| configuration  | loose | exact | lastdig | notes |
|----------------|-------|-------|---------|-------|
| serial `-Ofast`| 50/51 | 45    | 48      | baseline |
| serial `-O2`   | 50/51 | 46    | 48      | |
| MPI `-Ofast` ×1| 50/51 | 45    | 48      | **identical to serial** |
| MPI `-Ofast` ×2| 49/51 | 45    | 47      | 1 crash |
| MPI `-Ofast` ×4| 49/51 | 45    | 47      | 1 crash |
| MPI `-O2` ×1   | 50/51 | 46    | 48      | **identical to serial** |
| MPI `-O2` ×2   | 48/51 | 45    | 47      | 1 crash, 1 extra loose failure |
| MPI `-O2` ×4   | 49/51 | 45    | 47      | 1 crash |

**Only two tests move at all** across the whole matrix. Every other test in the short suite
produces the same worst-case relative error at every rank count and both optimisation settings.

| test | fast-math | MPI build (×1) | rank 1→2 | rank 2→4 |
|---|---|---|---|---|
| `h2o_rhf_cc-pVDZ_tdhf` | −0.154 | **0** | +0.183 | −0.527 |
| `nh3_rhf_DZP_HAR`      | +10    | **0** | 0        | 0        |

#### Finding 1 — the MPI build itself costs nothing numerically

`MPI -n 1` reproduces the serial build **exactly**, at both optimisation settings: same
`exact`/`lastdig` counts, and identical per-test worst-case errors. This was not a given —
`#ifdef MPI` strips `PURE`/`ELEMENTAL` from the entire codebase, which is a large codegen change.
It turns out to cost no accuracy. Practically, this means any difference observed at higher rank
counts can be attributed to reduction order rather than to the MPI build, which is what makes the
rest of the table interpretable.

#### Finding 2 — rank-count drift is real but confined

Only `h2o_rhf_cc-pVDZ_tdhf` shows rank-count-dependent drift:

| config | max rel % | max LDD |
|---|---|---|
| serial `-O2` / MPI `-O2` ×1 | 0.385 | 17 |
| MPI `-O2` ×2 | 0.568 | 51 |
| MPI `-O2` ×4 | 0.041 | 2 |

The movement is **non-monotonic** — it does not grow with rank count, it wanders. That is the
signature of an iterative solver (TDHF's Davidson) whose convergence path is perturbed by a
round-off-level change, not of an accumulating error. It is already in `suite_report.py`'s
`KNOWN_MARGINAL` list as runner-sensitive with a relaxed 0.5 % bound, and at `-O2` ×2 it exceeds
even that. No other test in the suite is affected.

#### Finding 3 — one hard crash at ≥2 ranks (FIXED)

`DWGN_lamaGOET_NBO_file_47` aborted at every rank count above 1, at both optimisation settings:

```
At line 3126 of file molecule.put.F90
Fortran runtime error: Unit number is negative and unit was not already
opened with OPEN(NEWUNIT=...)
```

A debug MPI build put it in `MOLECULE.PUT:put_NBO_file_47`, which calls
`stdout.redirect(...)` and then makes **thirteen raw Fortran `write(stdout.unit,…)` calls**,
reaching around the TEXTFILE API and so getting none of its `IO_IS_ALLOWED` guarding. Only the
master opens the redirected file (`TEXTFILE.open` is guarded), and `FILE.open` then broadcasts
its unit — negative, from `newunit=` — to every rank, so non-master ranks wrote to a unit they
had never opened.

Fixed by guarding the writes. Nothing needs broadcasting: this is write-only output, so the
simple guard is the whole fix. DWGN now passes at `-n 2` and `-n 4` **bit-exact** on all three
compared outputs, and is unchanged serially.

**The important lesson is about the failure mode, not this routine.** It crashed loudly only
because a *redirected* TEXTFILE holds a negative `newunit`. An unguarded raw write to a
**non-redirected** `stdout` would use `TEXTFILE_STD_OUT_UNIT` = 6, which is a valid preconnected
unit on every rank — so it would **silently interleave** output rather than crash. The bugs found
here are the lucky subset. See the audit below.

#### Finding 5 — raw Fortran I/O reaches around the abstraction

`FILE`/`TEXTFILE` are largely MPI-correct — notably `FILE` gets the hard part right, guarding
reads and broadcasting the **data** rather than the handle (`file.foo:211/215`, `244/248`,
`325/329`). But code reaches around them via `.unit`, and whether those sites are guarded is ad
hoc:

| site | status |
|---|---|
| `molecule.put.foo` `.wfn` writer (~20 statements) | correctly wrapped in one `IO_IS_ALLOWED` block |
| `crystal.foo:8826` | correctly guarded |
| `molecule.put.foo` `put_NBO_file_47` (13 statements) | **was unguarded** — fixed here |
| `plot_grid.foo:2280` | **fixed** — guarded *and* the result broadcast (this one needed both halves) |
| `archive.foo:2687, 2712, 2763` | **fixed** — master-only. Each rank had its own `newunit`, so no crash: all ranks wrote the *same filename* concurrently. Silent corruption. |

Because the silent regimes (preconnected unit 6; per-rank `newunit` on the same filename) do not
announce themselves, an audit by inspection is not enough. The durable fix is a **translator
lint**: flag any `write(`/`read(` on a `*.unit` expression outside `file.foo`/`textfile.foo`/
`buffer.foo`. That is static, cheap, and would have found every site above without running
anything. Folded into milestone 6.

#### Finding 4 — `-ffast-math` matters more than MPI does

Turning off `-ffast-math` changes more than any amount of MPI does: it moves one test to
bit-exact agreement (45 → 46 exact) and changes `nh3_rhf_DZP_HAR`'s worst relative error by 10
percentage points, while MPI changes it by nothing. (That test's huge relative figure is a
near-zero-denominator artefact — it passes on the last-digit criterion, which is why it carries a
relaxed bound.) The practical implication: when chasing a numerical discrepancy in an MPI build,
suspect the optimisation flags before the rank count.

#### Caveat on the metric

`max rel %` is the worst single token in a test's output, so "0 change" means the *worst* token
did not move; sub-threshold movement elsewhere is not excluded. The `exact` counts corroborate
the picture though: 45/46 exact matches are preserved at ×1 and only one is lost at ×2.

### Cross-platform replication: achari2 (Linux x86_64)

Repeated on `achari2` — Linux 6.8.0-88-generic, x86_64, gfortran **14.2.0**, Open MPI 5.0.9 built
from source with `FC=gfortran-14` (Ubuntu's packaged Open MPI is built against gcc-13, so it hits
the same `.mod` trap as Homebrew's — this is the *default* situation, not a macOS quirk).

| configuration | loose | exact | notes |
|---|---|---|---|
| serial `-Ofast` | **51/51** | 49 | |
| serial `-O2` | **51/51** | 46 | |
| MPI `-Ofast` ×1 | **51/51** | 48 | matches serial |
| MPI `-Ofast` ×2 | 50/51 | 48 | 1 crash (DWGN, since fixed) |
| MPI `-Ofast` ×4 | 50/51 | 48 | 1 crash |
| MPI `-O2` ×1 | **51/51** | 46 | matches serial |
| MPI `-O2` ×2 | 46/51 | 40 | **5 crashes** |
| MPI `-O2` ×4 | 46/51 | 41 | **5 crashes** |

**Confirmed on Linux:**

- **The macOS 50/51 was a platform artefact.** Linux is 51/51 serially, so
  `urea_ccsd_pob-TZVP_Salvador_properties` fails only on macOS arm64 against these
  Linux-blessed references. Nothing to do with MPI.
- **MPI at 1 rank reproduces serial**, on both platforms and at both optimisation settings. This
  is now a two-platform result, so the `PURE`/`ELEMENTAL` stripping genuinely costs nothing.
- **The π invariant gives bit-identical values on both platforms** — `3.141592653589362` /
  `…390` / `…147` at 1/2/4 ranks on arm64 macOS *and* x86_64 Linux. Reduction order is
  reproducible across architectures.
- **The DWGN crash reproduces on Linux**, so it was never macOS-specific.

### Finding 6 — undefined behaviour exposed only at `-O2`, only on Linux

Four *additional* tests crash on Linux, but **only** in the `-O2 -fno-fast-math` MPI build, and
only at ≥2 ranks: `c9o9h8_read_cif_IT_group_9`, `maleate_read_CIF_H_double_bond_new_BLs`,
`maleate_read_CIF_H_double_bond_old_BLs`, `urea_lamaGOET_grown_CIF`. All are CIF-reading jobs.

The failure is not an I/O guard problem:

```
*** An error occurred in MPI_Bcast
*** MPI_ERRORS_ARE_FATAL (processes in this communicator will now abort)
```

i.e. a **mismatched collective** — the ranks disagree about the broadcast, i.e. about its length
or about whether they participate at all.

Reproduced deterministically, same machine, same test, same rank count:

| build | result |
|---|---|
| `build-mpi-fast` (`-Ofast`) | passes |
| `build-mpi-o2` (`-O2 -fno-fast-math`) | `MPI_Bcast` error, rc=15 |

A bug that appears and disappears with the optimisation level, and on one platform but not the
other, is **undefined behaviour** — an uninitialised value or an out-of-bounds read feeding either
a broadcast length or the branch that decides whether a rank reaches the collective. This is the
"genuine UB" milestone 4 anticipated, and it is real.

**UPDATE 2026-08-02 -- substantially diagnosed.** The failing call is the `.IO_status` broadcast in
`TEXTFILE:read_line_external` (`MPI_BCAST(buffer,1,MPI_INTEGER,...)`) receiving a 256-character
message, i.e. paired with the adjacent `string` broadcast. Reached via `VEC{ATOM}:read_smcif` ->
`read_smcif_atoms_xtal` -> `CIF:find_looped_item` -> `TEXTFILE:look_for_item`. Re-enabling the 2021
`MPI_BARRIER` makes all three tests pass, which rules out a sequence-length mismatch (that would
deadlock) and points at the fact that every `PARALLEL_BROADCAST` is conditional on
`WORK_IS_SHARED` and can be silently skipped on one rank. See `DEFERRED.md`. What makes the ranks
disagree is still open.

The original note follows. It had **not** been diagnosed at the time of writing. Doing so needs a debug MPI build on Linux (`-fcheck=bounds`
plus `-finit-integer`/`-finit-real=snan` would likely name it immediately). Recorded rather than
guessed at. Note the practical consequence: `-Ofast` **hides** this, so the shipped configuration
is the one where it is invisible, not the one where it is absent.

### PARTLY diagnosed — the gate fix was necessary but NOT sufficient (2026-08-04)

**Retracted:** an earlier version of this section, written on 2026-08-04, declared this resolved
on the strength of `e3ef5906`'s commit message and the comment in `macros.in`. **It is not.**
Re-run on `achari2` (Linux x86_64) against current `antlr4` (`ed706c97`), in the same
`-O2 -fno-fast-math` MPI build, **all four tests still abort at `-n 2`** with the identical
`MPI_ERR_TRUNCATE` in `MPI_Bcast`, exit 15. `-n 1` passes with an *exact* match. The gate fix is
confirmed present in that build:

```
#    define PARALLEL_BROADCAST0(X,Y)      if (tonto%is_parallel) call broadcast_(tonto,X,Y)
```

So `e3ef5906`'s claim to "fix the four CIF tests" was over-stated -- it removed one real cause
(a collective gated on rank-local state, which was genuinely a bug and stays fixed), but at
least one other cause remains. Note also that the commit verified only *three* of the four.

The analysis below is retained because it is correct as far as it goes: the mechanism it
describes is real and was fixed. What follows it is the continuing hunt.

### Localised to a codegen bug in `textfile.F90` at `-O2` (2026-08-04)

Everything below is from `achari2` (Linux x86_64, gfortran 14.2.0, Open MPI 5.0.9 built with the
same compiler), `build-mpi-o2` = `-O2 -fno-fast-math`, test `c9o9h8_read_cif_IT_group_9` at
`-n 2` unless stated. **Each claim has a control**; the earlier round of this investigation went
wrong by inferring instead of measuring, so this time nothing is asserted without its opposite
being tested.

| experiment | result |
|---|---|
| no pin (control) | **FAIL** exit 15, 2/2 runs |
| `textfile.F90` pinned to `-O1` | **PASS** 3/3 runs, and all four CIF tests pass |
| one `write` probe inside `TEXTFILE:look_for_item` | **PASS** 5/5 runs |
| that probe removed again | **FAIL** — the mask is that one probe |
| a probe in `move_to_record_external` instead | FAIL — masking is specific to `look_for_item` |
| `textfile.F90` at `-O2 -fcheck=all -fbacktrace` | **PASS**, and **no check fires** |
| `textfile.F90` at `-O2 -fno-schedule-insns` | FAIL — *not* the arm64 `shell1quartet` pass |
| `-n 1`, any build | PASS, **exact** agreement |

**What the desync looks like.** Tracing every `MPI_BCAST` to a per-rank file (`fort.70`,
`fort.71` — never a shared stream, they interleave mid-line) shows the ranks agreeing for
**8,481** broadcasts of alternating `(MPI_CHARACTER 256, MPI_INTEGER 1)` pairs, then rank 0
issuing **one surplus integer broadcast** that rank 1 never issues. Everything after is offset by
one, and the next pair mismatches a 1-integer receive against a 256-character send:
`MPI_ERR_TRUNCATE`.

**A hypothesis that was tested and rejected.** `move_to_record_external` steps
`move_to_back_record` once per record, one broadcast each, so a `.record` differing by one
between ranks would produce exactly one surplus broadcast. Probing `.record` on both ranks at
every call: **it matches everywhere**. Rank 0 does make three `move_to_record` calls rank 1 never
makes, but they occur *after* the first divergence, so they are a consequence. Recorded because
the hypothesis is a good one and someone will have it again.

**Assessment.** `-fcheck=all` masks it and reports nothing, so this is not a source-level
out-of-bounds. A single unrelated `write` in one routine masks it. It behaves like a **gcc
miscompilation of `textfile.F90` at `-O2`**, not like a Foo-level bug -- which is why the
`-fcheck=bounds` / `-finit-*` plan the milestone originally proposed would have found nothing.

**Workaround available now**, and it is the one this project already uses twice (`types.F90` is
pinned to `-O1`; `shell1quartet.F90` to `-O2 -fno-schedule-insns` on arm64 macOS):

```cmake
set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/textfile.F90
    PROPERTIES COMPILE_OPTIONS "-O1")
```

`textfile.F90` is line-oriented I/O, not a hot path, so pinning it costs essentially nothing.
**Not yet committed** -- see the open questions.

**Open, for the next session:**

### CULPRIT FOUND: `-foptimize-sibling-calls` (2026-08-05)

Bisected with a 7-second per-file harness (recompile `textfile.F90` only, relink, run twice —
`~/m7fast.sh` on achari2; the CMake route took 20 minutes per iteration and made this
impractical). Of the **45** flags `-O2` enables over `-O1`, **`-foptimize-sibling-calls` is the
only one whose removal fixes it**:

| build | result |
|---|---|
| `-O2` | **FAIL** |
| `-O2 -fno-optimize-sibling-calls` | **PASS** (2/2, and all four CIF tests pass) |
| `-O1` | PASS |
| `-O1` + **all 45** `-O2`-only flags | **PASS** |
| `-O3`, `-Ofast`, `-Ofast -march=native` | **PASS** |
| only differing `--param` (`max-fields-for-field-sensitive` 0→100), forced back at `-O2` | FAIL |
| `-O2 -finit-integer/-real=snan/-logical` poisoning | FAIL (nothing reported) |

**This explains why the bug hid from inspection.** A sibling (tail) call is one where the caller's
stack frame is torn down *before* jumping to the callee, so the callee reuses it. Put any
statement after a call and it is no longer the last thing the routine does, so it is no longer a
tail call — which is exactly what a `write` probe or `-fcheck=all` does. The bug did not "move
when observed" in some mysterious way; **observing it removed the optimisation that caused it.**

**It is an interaction, not one bad pass.** `-O1` plus all 45 flags — sibling calls included —
passes, and `-O3`/`-Ofast`, supersets of `-O2`, also pass: their extra passes reshape the code
away from the trigger. So the **shipped release build (`-Ofast`) is not affected**, and no
released binary has been wrong. That is luck rather than immunity, which is why the workaround is
applied unconditionally.

**Workaround (committed).** `CMakeLists.txt` pins the one file:

```cmake
set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/textfile.F90
    PROPERTIES COMPILE_OPTIONS "-fno-optimize-sibling-calls")
```

Cost is nil — line-oriented I/O, never a hot path — and it restores the `-O2 -fno-fast-math`
control build, which §6 needs in order to separate floating-point reassociation from genuine MPI
defects.

**Still NOT established, and it matters:** *which* tail call, and whether the fault is a gcc bug
or latent UB in the Foo sources that only tail calls expose. The `tailc` dump shows 154 tail calls
in `textfile.F90`; the 14 that pass an address all pass `&C.NNNN` compiler constants in static
storage, not stack locals, so there is no smoking gun of the classic
"pointer into a dead frame" form. `read_line_external` is where the mismatch *surfaces* — it does
contain tail calls, but only on its `die()` paths, which abort anyway, so **it has not been shown
to be the cause**. Reducing this to a self-contained test case is the next step, and the
prerequisite for reporting it upstream.

### Ruled out along the way

1. **All eight obvious candidates.** Each was tried as
   `-O2 <flag>` on `textfile.F90` alone, 2 runs each, and **every one still failed** (exit 15):
   `-fno-schedule-insns`, `-fno-schedule-insns2`, `-fno-strict-aliasing`, `-fno-tree-vrp`,
   `-fno-gcse`, `-fno-tree-pre`, `-fno-code-hoisting`, `-fno-store-merging`. So it is not one of
   the classic single-pass culprits, and notably **not** the `-fschedule-insns` that the arm64
   `shell1quartet` workaround targets.

   **Bisect the other way next.** Subtracting from `-O2` means testing ~30 flags one at a time
   with no guarantee a single one is responsible. Start from `-O1` (known good) and *add* the
   `-O2`-only flags, binary-searching the set — `gcc -Q -O2 --help=optimizers` versus
   `-Q -O1 --help=optimizers` gives the exact difference for gfortran 14.2. That converges in
   ~5 builds instead of 30, and it also answers whether *any* single flag is responsible: if
   `-O1` plus the whole set still passes, the trigger is an interaction, which would point away
   from a simple miscompilation and back towards latent UB.

   Harness on achari2: driver `~/m7pin.sh "<flags>"` (edits the pin, rebuilds, runs the test
   twice), loop `~/m7bisect.sh`, log `/tmp/m7bisect.log`.
2. **Is `-Ofast` genuinely safe or merely lucky?** The shipped build has never failed here, but
   if this is a miscompilation that is luck, not immunity. Worth rebuilding `build-mpi-fast` on
   achari2 and re-running the four tests before deciding the pin is only needed at `-O2`.
3. **Is it really the compiler?** Before filing anything upstream, reduce it: the surplus
   broadcast comes from somewhere in `look_for_item`/`move_to_record_external`; a minimal
   reproducer would settle compiler-versus-source. Also worth trying gfortran 13 and 15 on the
   same file.

### The original diagnosis (correct, but not the whole story)

**It was never undefined behaviour.** That framing came from the symptom -- a failure that moves
with optimisation level and platform -- and it was wrong. The cause is deterministic and was
sitting in one macro:

```
#    define PARALLEL_BROADCAST0(X,Y)   if (DO_IN_PARALLEL) call broadcast_(tonto,X,Y)
```

`DO_IN_PARALLEL` (now `WORK_IS_SHARED`) is `is_parallel AND parallel_do_lock == " "`, and **the
lock is rank-local state**: it is set by executing a loop body, which a rank handed zero
iterations by the cyclic distribution never does. So two ranks could disagree about whether to
enter a broadcast. MPI pairs collectives by issue order, so one skipped broadcast offsets the
streams and the *next* pair mismatches -- a 1-integer receive against a 256-character send, i.e.
the `MPI_ERR_TRUNCATE` seen in `TEXTFILE:read_line_external`.

Optimisation level and platform only changed *whether the ranks happened to diverge*, not whether
the bug was there. Nothing was uninitialised and nothing was out of bounds, which is why the
`-fcheck=bounds` / `-finit-*` plan suggested above would have found nothing.

The fix is the one-line gate change: a **broadcast** or **barrier** is gated on `is_parallel`
alone, while a **reduction** stays gated on `WORK_IS_SHARED`. The asymmetry is deliberate --
skipping a reduction under a held lock is correct (there are no rank-partitioned partials to
combine), skipping a broadcast never is. The general rule:

> Whether a **collective** executes must never depend on state that can differ between ranks.

**Guarded, because the shipped build cannot expose a regression.** `release` is `-Ofast`, which
hid this, so no CI job can catch it coming back. `scripts/check_parallel_lint.py` therefore audits
the gates in `macros.in` directly: broadcasts and barriers must be gated on `is_parallel` and must
not mention `work_is_shared`; reductions must be gated on `work_is_shared`. Verified to fail
against the exact pre-fix definition. It runs as the `parallel_lint` ctest, label `short`, so it
is in CI.

**Verification status.** `e3ef5906` records: *"Verified on achari2 (Linux x86_64): all three
tested pass at -n 2, -n 1 unaffected"* -- three of the four tests, in the `-O2 -fno-fast-math`
build where they failed. Outstanding: the fourth test, and a re-run of all four on Linux at `-O2`
against current `antlr4`, which has changed substantially since (the I/O broadcasts moved to
`PARALLEL_BROADCAST_IO`, `TEXTFILE:flush` was fixed, per-rank I/O now actually works).

### Finding 7 — the suite is nondeterministic, and the one thing that is not (2026-08-26)

Found while moving CI to gfortran-16. It is recorded first because it invalidates a habit,
not just a number: **a single MPI suite run is not evidence.**

Five runs of `ci-mpi.yml`, all at commit `ed25349b`, same compiler, same cached Open MPI:

| run | ERROR | loose | exact |
|---|---|---|---|
| 32968421834 | **11** | 42/55 | 36 |
| 32976459938 | 1 | 52/55 | 47 |
| 32976469015 | 1 | 52/55 | 50 |
| 32976478217 | 1 | 52/55 | 50 |
| 32976487120 | 1 | 52/55 | 50 |

Read it carefully, because the obvious reading is wrong. This is **not** per-test flakiness:

- `urea_read_and_process_CIF` errors in **5 of 5** (6 of 6 including the run after) — it is a
  **deterministic** failure with a stable reproducer.
- The other ten errors happened **together, in one run, and never again**. Ten tests failing at
  once and then not at all is one event, not ten flaky tests.
- Even among the four ERROR-1 runs, `exact` varies 47–50, so the *numbers* drift run to run
  independently of the errors.

Ruled out for the burst: the Open MPI cache (all five restored the same one) and runner
contention (the burst run ran **alone**; the four concurrent ones were clean).

**What this costs.** A `50/55` under gfortran-14 was compared against a `52/55` under
gfortran-16 and reported as an improvement. It was not evidence — the spread on identical
code is wider than the effect. gfortran-16 does sit at ERROR 1 in four of five runs against
gfortran-14's 3, but gfortran-14 has **n=1** and no such claim can be made yet.

**Correction to Finding 6's practical consequence.** That section says `-Ofast` hides the
CIF-test failures and `-O2` exposes them. CI is `-Ofast` and the same family errors there, so
the right statement is that `-Ofast` **hides it most of the time, not always**. The `-O2` /
`-foptimize-sibling-calls` bisection stands; the inference that `-Ofast` is clean does not.

#### Why this went unseen, which matters more than the finding

Three layers each discarded the evidence, each assuming another kept it:

1. `ci-mpi.yml` piped the report through `| tail -30`, so **45 of 55 rows never reached the
   log**. The full table went only to `$GITHUB_STEP_SUMMARY`, which is not reachable through
   the API. Three green runs had already gone by.
2. An ERROR row prints `ERROR ERROR ERROR - -` and nothing else.
3. `suite_report.py` ran each job with `capture_output=True` and the ERROR branch **dropped
   both streams**, building its verdict from the return code alone. A crashed job also writes
   no `.bad`, so the artefact upload had nothing to collect.

The result was eleven failures with **no recorded cause anywhere**, which forced attribution by
test *name* — guessing. Fixed: `suite_report.py --failure-dir` writes one untruncated log per
ERRORing test (command, exit status, both streams) and CI uploads it; the `tail` is gone.
**Rule: truncate for display, never for capture.**

#### ROOT CAUSE (2026-08-26): a collective count driven by rank-local state

`urea_read_and_process_CIF` at 2 ranks. Reproduced **5/5** locally on macOS/arm64 with
Homebrew GCC 16.1.0 at `-Ofast`, and on Linux/x86_64 with the 16.0.1 snapshot in CI — so it
is neither platform- nor optimisation-specific. `-n 1` passes **exactly** (0%, 0 ulp).

**`TEXTFILE:move_to_record_external`** (`foofiles/textfile.foo:1263`):

```fortran
if (rec < (.record+1)) then
   do ; .move_to_back_record ; if (rec==(.record+1)) exit ; end
else if (rec > (.record+1)) then
   do ; .move_to_next_record ; if (rec==(.record+1)) exit ; end
end
```

Each iteration of either loop issues one `PARALLEL_BROADCAST_IO`. **The iteration count is
computed from `.record`, which is rank-local**, so two ranks that disagree about `.record`
issue different numbers of collectives. That is the milestone-7 rule violated verbatim:
*whether a collective executes must never depend on state that can differ between ranks.*

And nothing puts them back in step. **`.record` is broadcast in exactly one place in the
file — `textfile.foo:3550`, in the `flush` (write) path.** The read path maintains it purely
locally, `+1` in `move_to_next_record` and `-1` in `move_to_back_record`. So the loop count
depends on `.record`, and `.record` is only kept correct by executing equal loop counts: one
divergence is permanent and self-amplifying.

**The observed damage.** Once the streams shift by one, every later collective binds to the
wrong variable. Rank 1 receives a *record counter* into `.IO_status` — the trace shows the
values 113, 114, 115 cycling — sees `114 /= 0`, and dies at the `DIE_IF` in
`move_to_next_record`. Master is unaffected and is already writing `urea.cxc`, which comes
out truncated at 12–16 lines of 101 when `MPI_ABORT` lands.

**Rank 1's stack**, obtained with `lldb` on the *release* binary, no rebuild:

```
SYSTEM:die <- SYSTEM:die_if <- TEXTFILE:move_to_next_record <- TEXTFILE:move_to_record
           <- TEXTFILE:move_to_line <- CIF:move_to_end_of_data
           <- MOLECULE.CE:process_cif_for_cx <- MOLECULE.MAIN:read_keywords
```

The debug build's `ENSURE(.file.is_open)` fires at `CIF:move_to_end_of_data` — frame 5 of that
same stack. **Debug and release are one failure, not two.**

##### Three method lessons, all of which cost time here

1. **A broadcast trace of `(length, datatype, value)` cannot detect misalignment.** Rank 1's
   *n*-th receive *is* master's *n*-th send, so both traces record the same value whatever
   variable each rank binds it to. The two streams came out byte-identical and were briefly
   read as "perfect lockstep" — the opposite of the truth. To see a shift, a trace must carry
   a **call-site identity**, not the payload. 66% of broadcasts here are `1 x MPI_INTEGER`, so
   shape alone is useless too.
2. **`lldb` on the optimised binary answered in two minutes what two 30-minute instrumented
   rebuilds did not.** Break on `die`, not `die_if` — `DIE_IF` expands to
   `call die_if_(tonto,cond,msg)`, so a breakpoint there fires on every evaluation including
   the benign ones, and the first stack you get is a startup check.
3. **The diagnostic text is wrong and cost a wrong turn.** `move_to_next_record` opens
   nothing; `iostat/=0` there is EOF or a read error. "error opening new file" appears at
   **seven** sites in `textfile.foo` and one in `file.foo`. It should name the operation and
   print the rank and the `iostat` value.

##### PARTIAL FIX APPLIED (2026-08-26) — an amplifier removed, the origin still open

`move_to_record_external` now positions the file on the I/O rank alone and broadcasts the
result, which is the standard pattern:

```fortran
if (IO_IS_ALLOWED) then
   ... backwards / forwards loop, both movers now NON-collective ...
end
PARALLEL_BROADCAST_IO(.IO_status,tonto.master_processor)   ! exactly one, every rank
PARALLEL_BROADCAST_IO(.record,   tonto.master_processor)   ! exactly one, every rank
DIE_IF(.IO_status/=0,"error moving to a record in "//trim(.name))
```

`move_to_next_record` and `move_to_back_record` lost their collectives entirely; each has
exactly one caller, so no `_io` twin was needed. Both carry a comment saying they must stay
non-collective. The loops gained an `exit` on `.IO_status/=0`, because the `DIE_IF` that used
to terminate a failing loop now lives in the caller. Collective traffic also drops from one
pair per record stepped to one pair per positioning call.

Positioning on master additionally dissolves a problem that broadcasting inside the loops
would **not** have fixed: `rec` is not independent of `.record` (`cif.foo:712` sets
`.end_of_data = .file.record`), so had every rank kept looping they would have disagreed
about the *target* as well as the position. Non-master ranks now never evaluate the exit
condition at all.

**It did not cure the test, and the causality was the other way round from what is written
above.** After the fix, `urea_read_and_process_CIF` at `-n 2` fails 3/3 with

```
*** An error occurred in MPI_Bcast
*** MPI_ERR_TRUNCATE: message truncated
```

`-n 1` and serial still pass exactly. So `move_to_record_external` was an **amplifier**, not
the origin: the ranks were *already* one collective apart on entry, which is what made
`.record` diverge. The divergent `.record` was a symptom that had been read as the cause. With
the amplifier gone the underlying shift surfaces honestly as `MPI_ERR_TRUNCATE` — milestone
7's original signature — instead of as a bogus "error opening new file".

The fix stays: a collective count driven by rank-local state is a defect on its own terms, it
sits on the path of every file read, and it was disguising the real failure.

**The origin is upstream of `move_to_record_external`** — in whatever first put the ranks one
collective apart, before `CIF:move_to_end_of_data` is reached. From rank 1's stack that means
`CIF:find_end_of_data_block` / `MOLECULE.CE:process_cif_for_cx`.

##### PARTIAL VERIFICATION DONE (2026-08-26, late) — the `long` suite, macOS, 1 rank

Run after the section below was written, so read this first. The **`long` suite** was run
against the fixed binary: **28/31 loose**, and **all three non-passes are known and
pre-existing**, none of them caused by the `textfile.foo` change:

| test | why it fails, and since when |
|---|---|
| `quartz_NN_HAR_L0_rhf_def2-SVP` | macOS-only, reference is *correct* — `DEFERRED.md`, "fail on macOS only" |
| `quartz_NN_HAR_L1_rhf_def2-SVP` | same |
| `ammonium_borane_pHAR_C23` | ERROR — the test blocked by the missing 167 MB LFS asset, its own deferred entry |

So the `long` jobs — the ones no workflow runs, and the heaviest users of the CIF and archive
reading this change touches — show **no regression**.

**What this does NOT discharge, and the distinction matters.** It was run with
`build-mpi-local` (macOS/arm64, Homebrew GCC 16.1.0, `-DMPI=1`, `-Ofast`) at **`-n 1`**, not
with a serial gfortran-14 build on Linux. At one rank `IO_IS_ALLOWED` is always true so the
new guard is a no-op and the changed control flow is exercised exactly as it would be in
serial — which is the point — but the compiler, the platform and the macro configuration all
differ from the baseline in `CLAUDE.md` §5. Treat this as strong evidence of no regression in
the `long` jobs, not as the owed suite run.

**Still owed:** `short long hart` on Linux, gfortran-14, serial release, against the 124/124
baseline. That is now runnable without a machine — `gh workflow run ci-full-suite.yml --ref
develop`, see `.github/workflows/ci-full-suite.yml`.

##### VERIFICATION STILL OWED — run the SERIAL suite before this goes near `master`

**Not yet run as of 2026-08-26.** `foofiles/textfile.foo` is on the path of **every file read
in Tonto**, so `move_to_record_external` and the two record movers are exercised by essentially
every job, serial included. The blast radius of that change is the whole suite, not the one MPI
test it was aimed at. What has been run is only: the reproducer at `-n 2` (still fails,
`MPI_ERR_TRUNCATE`), and `-n 1` plus a serial run of `urea_read_and_process_CIF` alone (both
pass exactly, 0%, 0 ulp). That is three jobs out of 124.

The change is *structural* rather than numerical — it moves work under an `IO_IS_ALLOWED`
guard, which is a no-op in a serial build, and moves a `DIE_IF` out of a loop into its caller.
So the expectation is no change at all in serial. **That expectation is exactly what has to be
tested, not asserted** — a loop whose terminating `DIE_IF` moved is precisely the kind of edit
that can turn a clean failure into a silent one, and the new `exit` on `.IO_status/=0` is a
control-flow path that did not exist before.

```bash
# a release build from the current sources, then the full suite
cmake -B build -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
cmake --build build -- -j3            # -j3, not -j$(nproc): one JVM per .foo file
python3 scripts/suite_report.py --program build/tonto --tests-dir tests \
        --basis-sets basis_sets --suites short long hart \
        --failure-dir test-failures --log tests.log
```

**The baseline to compare against** — from `CLAUDE.md` §5, measured before any of this work:
full release suite **124/124** loose locally; short suite **51/51** in CI. Anything below that
is a regression from this change and must be treated as one. Use `--failure-dir`: an ERRORing
job writes no `.bad`, so without it the cause is recorded nowhere (that is register row 11).

A second, cheaper gate that is *not* a substitute: `ctest -L short` on the same build. It is
what CI runs and it will catch a gross breakage in minutes, but it does not exercise the `long`
jobs where the heavier CIF and archive reading lives — which is the code this change touches.

Until the serial suite is green at the baseline, treat the `textfile.foo` change as **unproven
outside three jobs**, whatever the MPI story does.

##### How to find it: the probe must carry CALL-SITE identity

The lesson from the failed attempt above, stated as a method: **do not trace payloads, trace
call sites.** Rank 1's *n*-th receive *is* master's *n*-th send, so a trace of
`(length, datatype, value)` is identical on both ranks no matter which variable each binds —
it cannot see a one-step shift, and it was briefly misread as proof of lockstep.

Two ways to get call-site identity:

1. **`LD_PRELOAD` a PMPI shim — no Tonto rebuild at all, Linux only.** MPI defines the
   profiling interface, so a small shared library can define `MPI_Bcast` (and the Fortran
   `mpi_bcast_`), record the caller's return address via `backtrace()`, call `PMPI_Bcast`,
   and write one line per call to a **per-rank** file. Resolve the addresses afterwards with
   `addr2line`. Diff the two rank files: the first differing call site is the origin. This is
   the cheapest option by far and needs no rebuild, no probe in `parallel.foo`, and no
   recompilation between iterations.
2. **Emit `__FILE__`/`__LINE__` from the macro.** `PARALLEL_BROADCAST_IO` in
   `include/macros.in` expands *at each call site*, unlike the template in `parallel.foo`
   which is one source location for all 25 overloads. That is where call-site identity can be
   captured in-tree — but it costs a full rebuild per iteration.

Prefer (1). See "Debugging this on Linux" below.

##### Debugging this on Linux — recommended, and better than macOS

The bug reproduces on both platforms (Linux/x86_64 with the gfortran-16.0.1 snapshot in CI,
macOS/arm64 with Homebrew 16.1.0 locally), so either will do — but **Linux is the better
place to finish it**, for reasons that are practical rather than aesthetic:

- **`LD_PRELOAD` works properly.** The PMPI shim above is the whole ballgame: call-site
  identity with no rebuild, so each experiment costs seconds instead of half an hour. macOS
  has `DYLD_INSERT_LIBRARIES`, but System Integrity Protection strips it from protected
  binaries and the two-level namespace makes symbol interposition unreliable. On Linux it
  simply works.
- **`backtrace()`/`backtrace_symbols()` are in glibc**, so the shim can capture a stack
  without extra dependencies. macOS has `backtrace()` too, but resolving Fortran symbols in
  an `-Ofast` binary is poorer.
- **`gdb` handles gfortran better than `lldb`.** Module symbols, array descriptors and
  derived types are all more legible; `gdb`'s `--args` plus `mpirun -n 2 xterm -e gdb ...`
  or `gdb -p` attach per rank is a well-trodden path.
- **`achari2` already has the toolchain** — gfortran-16 and a working MPI — and it is the
  machine where the earlier `-O2` bisection was done, so the results stay comparable.

Practical notes for whoever does it:

- Give each rank its **own** output file, always. `TONTO_DEVELOPER_INFO.md` §1a, and it is the one
  rule that has never yet failed to matter here.
- Run at exactly `-n 2`. The failure needs a peer rank and nothing more; higher counts add
  noise and, for fragHAR, change the scheduler shape.
- `-n 1` and serial are the controls and both pass **exactly** (0%, 0 ulp) — if they ever
  stop passing, the change under test is wrong, independently of the desync.
- `mpirun --output tag` prefixes every line with `[job,rank]`, which is how it was
  established that rank 0 never dies. Cheap and worth doing first.
- Break on `die`, **not** `die_if`: `DIE_IF` expands to `call die_if_(tonto,cond,msg)`, so a
  breakpoint on it fires for every evaluation and the first stack you get is a benign startup
  check.

##### Fix direction (not yet implemented)

Make the collective count independent of rank-local state: decide the number of iterations on
the IO rank, broadcast **that**, and have every rank loop the same number of times — or
position the file on master alone and broadcast the resulting `.record`. Do **not** add a
barrier: it masks the shift instead of removing it. The commented-out `MPI_BARRIER` in
`parallel.foo`, with a note by Florian describing exactly this symptom ("BCAST interferes with
a different kind leading to str and Int ... to screw up communication"), was an earlier
encounter with this bug; it has been removed in favour of a one-line pitfall pointing here,
because commented-out code that hides a live defect is an invitation to re-enable it.

#### The deterministic one, with its cause

First run with `--failure-dir` produced this immediately:

```
Error on rank 1: TEXTFILE:move_to_next_record ... error opening new file urea.cif
MPI_ABORT was invoked on rank 1 in communicator MPI_COMM_WORLD
```

**The message is wrong and will mislead the next reader as it misled this one.**
`TEXTFILE:move_to_next_record` (`foofiles/textfile.foo:1316`) opens nothing — it forward-spaces
one record:

```fortran
.IO_status = 0
if (IO_IS_ALLOWED) then
   read(unit=.unit,fmt="()",iostat=.IO_status)   ! only the IO rank reads
end
PARALLEL_BROADCAST_IO(.IO_status,tonto.master_processor)
DIE_IF(.IO_status/=0,"error opening new file "//trim(.name))
```

A non-zero `iostat` here is **end-of-file or a read error**, not a failed open. And rank 1 never
reads: its `.IO_status` arrives by broadcast.

Two candidate mechanisms, **not yet distinguished**:

- **(a) A genuine read failure on the IO rank**, faithfully broadcast. Then master hit EOF too and
  both ranks die — plausible, since `PARALLEL_BROADCAST_IO` is gated on `is_parallel .and. .not.
  per_rank_IO_allowed`, which is uniform across ranks.
- **(b) A desync**, delivering a stale value to rank 1 — the Finding 6 class.

**The discriminator is whether rank 0 reports the same error.** Only rank 1's message survived,
because `MPI_ABORT` killed the job — that is consistent with either. Settle it with the per-rank
trace recipe in `TONTO_DEVELOPER_INFO.md` §1a, not by reading the code: three wrong readings of this
same area are already on record. Two cheap first steps: correct the diagnostic text so it names
what actually failed, and print the rank and `iostat` value with it.

### Defect register

Every MPI defect found, and whether it announces itself. **"Silent" is the dangerous column** —
those produce wrong numbers or corrupt files with no error at all.

| # | Site | What goes wrong under MPI | Loud? | Status |
|---|------|---------------------------|-------|--------|
| 1 | `molecule.grid.foo` ×4 | Reduction written inside its own `parallel do` → dead code; each rank keeps 1/N of the answer | **Silent** | **Fixed** |
| 2 | `parallel.foo` `parallel_symmetric_sum_23` | Triangle buffer sized from `dim1` not `dim2` → heap overflow on every call | **Silent** | **Fixed** |
| 3 | `molecule.fock.foo` ×3 | CIS/TDHF: `F`/`K` never reduced, or a reduced quantity mixed with a per-rank one | **Silent** | **Fixed** |
| 4 | `molecule.put.foo` `put_NBO_file_47` | 13 raw writes on a *redirected* unit → negative unit | Loud (crash) | **Fixed** |
| 5 | `plot_grid.foo:2280` | `read(textfile.unit,*)` on every rank; result also needed broadcasting | Loud | **Fixed** |
| 6 | `archive.foo` ×3 (VAPOR/stream/VTK) | Every rank opens the **same filename** with its own valid unit | **Silent** (corruption) | **Fixed** |
| 7 | `molecule.har.foo:1346` | `per_rank_write` in a **serial** loop — same file from every rank | Loud | **Fixed** |
| 7b | `crystal.foo:4961` `shift_update_ff` | same, and a read-modify-write of the shared file | Loud | **Fixed** |
| 7c | `get_Hirshfeld_atom_FFs_disk` | no barrier between the scattered writes and the collective reads | Race | **Fixed** |
| 8 | `system.foo:260` | Seeds not cloned; two broadcasts inside a master-only guard | Silent now, **deadlock** if naively "fixed" | Open |
| 10 | `textfile.foo:1263` `move_to_record_external` | Loop count -- and so the number of collectives -- computed from rank-local `.record`; `.record` is resynchronised only in the *write* path (`:3550`), never on read. Ranks shift by one and every later collective binds the wrong variable | Loud (abort), but only at >=2 ranks | **Open** (root cause found 2026-08-26) |
| 10a | `textfile.foo:1316` `move_to_next_record` | `urea_read_and_process_CIF` dies on rank 1 at 2 ranks, deterministically. Mechanism not yet distinguished (real EOF vs desync) — see Finding 7 | Loud (abort) | **Open** |
| 10b | same | Diagnostic says "error opening new file" for a routine that only *reads a record*, and prints neither the rank nor `iostat` | Misleading | **Open** |
| 11 | `ci-mpi.yml`, `suite_report.py` | ERROR cause captured then discarded; suite table truncated to the last 30 lines → failures with no recorded reason | **Silent** | **Fixed** |
| 9 | `system.foo:564` | `MPI_ABORT` commented out → one rank dying hangs the whole job | Hang | **Fixed** |
| 12 | `textfile.foo` `flush` | `.clear_and_put_margin` called **twice on master**, once elsewhere; it broadcasts, so the ranks desynchronise | Loud (`MPI_ERR_TRUNCATE`) | **Fixed** |
| 13 | `system.foo` `die` ×3 | error message written only under `IO_is_allowed`, so a dying **non-master** rank said nothing at all | **Silent failure** | **Fixed** |
| 14 | `run_har.foo` `--fos 0` | `set_F_sigma_cutoff(0)` violates its own `ENSURE`; worked in release only because the check compiles away | Loud in debug only | **Fixed** |
| 15 | fragment path | a **second** `MPI_ERR_TRUNCATE`, at *"Making F_pred"*, only under fragHAR | Loud | Open |
| 10 | `parallel.foo:6452` | `fragment_SCF_para` RMA: out-of-bounds read on the terminating fetch, every run | **Silent** | Open |
| 11 | `molecule.prop.foo:6118` | QTAIM: out-of-bounds at `nprocs==1`, sends to a non-existent rank, `MPI_FINALIZE` mid-run | Loud | Open |

### Status 2026-08-03: `hart` works under MPI

`hart` now runs to completion at 2 ranks and reproduces a serial run **digit for digit** —
release-serial against debug-MPI, so the agreement spans two different builds:

| | serial | MPI, 2 ranks |
|---|---|---|
| R(F) | 0.037995 | 0.037995 |
| N_r / N_p | 817 / 27 | 817 / 27 |
| GoF | 7.038231 | 7.038231 |

**fragHAR at 2 ranks still fails** (row 15), but 884 lines in — past the CIF, the atom groups and
the ANO data — rather than at the banner.

**The method that found row 12**, worth reusing for row 15 and any collective desync:

1. Trace every `MPI_BCAST`'s element count from the `PARALLEL:broadcast` *template*, so all 25
   type instantiations are covered by one edit.
2. Run under `mpirun --tag-output -n 2`, split by rank, and **diff the two length sequences**.
   `MPI_ERR_TRUNCATE` *is* a length mismatch, so the first differing index is the divergence.
3. Add `PHASE` markers printing the running broadcast count to find which interval contains it.
4. Then trace the arguments of whatever routine that interval implicates.

Two dead ends, recorded so they are not repeated: gfortran's `backtrace()` cannot symbolise on
macOS, and `FILE:close_and_delete`'s missing broadcast (a real bug, fixed on its own merits) was
**not** the cause.

### Why they hid, and what actually closes each class

Fixing the sites one by one is not the point — they were all *findable in principle* and none had
been found, because nothing made them announce themselves. Each class needs a different defence:

| Class | Why it was invisible | Defence |
|-------|----------------------|---------|
| Dead reductions (1–3) | `PARALLEL_*` is a silent no-op while the loop lock is held, so a reduction in the loop body reads as careful code and does nothing | **`parallel do … reduce(x)`** — the translator emits it in the one correct place, so it cannot be misplaced |
| Raw I/O on a redirected unit (4, 5, 7) | Nothing hid it — it crashes. But only once somebody actually runs MPI, which had never happened | Guard, plus a **translator lint** on `write(`/`read(` over any `*.unit` |
| Raw I/O on preconnected unit 6 | `TEXTFILE_STD_OUT_UNIT` is valid on *every* rank, so output is silently duplicated rather than failing | **Invalid `stdout.unit`/`std_err.unit` on non-master** in `create_stdout`/`create_std_err` — turns this class into the loud one |
| Raw I/O with its own `newunit` (6) | Each rank holds a genuinely valid unit; only the *filename* collides | Lint, plus per-rank filenames where parallel writing is actually intended |
| Collectives in rank-dependent branches (8) | Currently unreachable, so it looks fine — and becomes a deadlock the moment the ordering is corrected | **Abort on a suppressed collective** under `USE_PRECONDITIONS` |

The three defences in bold are milestone 6. Between them they close every row in the register,
including the ones not yet fixed — which is the argument for doing milestone 6 rather than
continuing to fix sites individually.

### Conclusion

Excluding the pre-existing platform failure, the MPI build is **as accurate as serial at 1 rank
and materially so at 2 and 4**, with one genuinely rank-dependent test (TDHF, already known to be
runner-sensitive). The one hard crash has been fixed, so the short suite now stands at **50/51
under MPI — the same as serial**, the single remaining failure being the pre-existing macOS
platform difference.

Reduction-order drift, the thing this milestone set out to measure, is present but far smaller
than expected — smaller than the effect of the compiler's own `-ffast-math` reassociation. The
real finding was not drift at all: it was eight wrong-answer reduction bugs and a set of raw-I/O
sites, in code that had never once been executed.

**Recommendation.** MPI is usable for the SCF/property paths exercised by the short suite. It is
**not** yet safe for anything driving `plot_grid`'s points file or `archive.foo`'s VAPOR/stream/
VTK writers (Finding 5), nor for HAR, whose `parallel_write` path writes the same file from every
rank (`DEFERRED.md`). Those need the audit in milestone 6.

