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
`ppa:ubuntu-toolchain-r/test`). The measurements recorded further down this page were taken
under **gfortran-14** and are left as measured — re-running them under 16 is the point of the
first scheduled run after the switch.

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
Pitfall 8 in `docs/TONTO_DEVELOPER.md` §1a, with the trace-based recipe that found it.

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

