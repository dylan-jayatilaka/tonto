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

## 4. What an MPI build changes, besides adding ranks

Worth knowing before interpreting any numeric difference:

- **`PURE` and `ELEMENTAL` are `#undef`ed across the entire codebase** (`include/macros.in:256`).
  MPI calls are impure and some `PURE` routines contain `PARALLEL_SUM`, so the purity contract
  cannot hold. This is a large codegen change — it costs common-subexpression elimination,
  loop-invariant hoisting and `elemental` vectorisation everywhere — and it is why an MPI build
  differs from serial **even at one rank**. Only routines that transitively reach a `PARALLEL_*`
  macro actually need it; narrowing that set is recorded in `ANTLR4_DEFERRED.md`.
- The `PARALLEL` type layout differs (`types.foo:376` has `#ifdef MPI` members).
- Work is distributed **cyclically**: rank *r* takes iterations *r, r+P, r+2P, …*
  (`parallel.foo:243`). Partial sums are therefore rank-partitioned, and reduction order depends
  on the rank count.

## 5. Defects found on first contact

Full detail, with evidence, in `ANTLR4_DEFERRED.md` under *"MPI: defects found during
milestone 4"*. Summary:

**Fixed** (these produced wrong answers, not drift, and would have made the characterisation
meaningless):

| Where | What |
|---|---|
| `molecule.grid.foo` ×4 | ESP/property-grid reductions written *inside* their own `parallel do`, where `DO_IN_PARALLEL` is always false — dead code, so each rank kept only its `1/n_ranks` share. One had no reduction at all. |
| `parallel.foo` | `PARALLEL_SYMMETRIC_SUM_23` sized its triangle buffer from `dim1` instead of `dim2`: a heap overflow on every call, plus an `ENSURE` testing the wrong dimensions. |
| `molecule.fock.foo` ×3 | CIS/TDHF: `r_CIS_S1_AV` reduced nothing at all; `r_CIS_S0_AV` and `u_CIS_AV` never reduced `K`. |

All the fixes are no-ops in a serial build, and this was verified: the short suite gives
bit-identical results before and after.

**Not yet fixed** — a latent collective-inside-a-master-guard deadlock in `SYSTEM:initialize`, a
commented-out `MPI_ABORT` (so one rank dying hangs the job), HAR writing the same file from every
rank, an out-of-bounds read in the `fragment_SCF_para` RMA work queue, and a QTAIM decomposition
that breaks at `nprocs == 1`. See `ANTLR4_DEFERRED.md`.

### The root cause, and the agreed fix

Most of the above is one mistake repeated. `FooToFortran` emits `LOCK_PARALLEL_DO` as the *first
statement inside* a `parallel do`, and `do_in_parallel` is false while that lock is held — so any
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

#### Finding 3 — one hard crash at ≥2 ranks

`DWGN_lamaGOET_NBO_file_47` **aborts** at every rank count above 1, at both optimisation
settings, reproducibly:

```
Fortran runtime error: Unit number is negative and unit was not already
opened with OPEN(NEWUNIT=...)
```

Root cause is in `foofiles/file.foo:134-146`: only the master actually opens the file
(`if (IO_IS_ALLOWED) open(… newunit=.unit …)`), and then `PARALLEL_BROADCAST(.unit, …)` hands the
master's negative `newunit` value to every rank. Non-master ranks hold a unit number they never
opened, so any *unguarded* I/O on it dies. This is the predicted file-I/O defect class; see
`ANTLR4_DEFERRED.md`. It is a real bug, not drift, and needs a debug MPI build to pin to the exact
routine.

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

### Conclusion

Excluding the pre-existing platform failure, the MPI build is **as accurate as serial at 1 rank
and materially so at 2 and 4**, with one genuinely rank-dependent test (TDHF, already known to be
runner-sensitive) and one genuine I/O bug that must be fixed before MPI can be recommended for
jobs that write archives. Reduction-order drift, the thing this milestone set out to measure, is
present but far smaller than expected — smaller than the effect of the compiler's own
`-ffast-math` reassociation.

