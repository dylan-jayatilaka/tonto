# Building Tonto on macOS

The same toolchain as Linux, installed with [Homebrew](https://brew.sh) instead
of `apt`. Everything here is one pass from a clean machine to a tested binary.
Other platforms: [Linux](BUILDING_ON_LINUX.md),
[Windows/WSL](BUILDING_ON_WINDOWS.md).

---

## 1. Install the prerequisites, with Homebrew

First Apple's command-line tools (`git`, `make`, a C compiler):

```bash
xcode-select --install
```

Then the rest:

```bash
brew install gcc cmake openjdk python3 numpy gnuplot openblas lapack
```

- `gcc` provides **`gfortran`**. This project standardises on **`gfortran-14`**.
  If Homebrew's `gcc` formula gives you a different version, `brew install gcc@14`
  and point CMake at it explicitly, as below.
- `openjdk` provides **`java`/`javac`** for the ANTLR4 `foo`→Fortran
  translator. If the build cannot find `javac`, add Homebrew's openjdk to your
  `PATH` as `brew` instructs — on Apple Silicon:
  ```bash
  echo 'export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"' >> ~/.zshrc
  ```
- `openblas` is the BLAS and LAPACK a `release` build uses. Without it the build
  falls back to Apple's Accelerate, whose LAPACK dates from 2009 and gives
  noticeably different eigenvectors. `lapack` is the netlib library a
  `reference` build needs, to compare with the stored test outputs.
- **`gnuplot` is needed at *run* time, not build time**, to render the
  diagnostic plots a refinement writes. Without it the job still completes and
  the data files and gnuplot scripts are still written; you get a warning and
  no pictures.
- `python3` runs the test harness, and `numpy` the Lebedev grid check among the
  self-checks. Without numpy that check reports itself skipped.
- Optional parallel build: `brew install open-mpi` — see the compiler-matching
  rule below.

## 2. Get the source code

```bash
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto
# master is the stable branch, and is what you get by default
```

`--recursive` pulls the submodules.

## 3. Configure and build

Tonto builds **out of source**: make a build directory, configure it once, then
`make`.

```bash
mkdir build && cd build
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
make -j4
```

If `gfortran-14` is not on your `PATH` under that exact name, point CMake at
what Homebrew installed — `ls $(brew --prefix gcc)/bin/gfortran*` will show it.

> **About `-j`.** Translation runs one JVM per `.foo` file, which is
> memory-heavy. A bare `make -j` (unbounded) can thrash the machine — cap it:
> `make -j4`, and lower it first if a build stalls.

You now have **`build/tonto`** and **`build/hart`** (`hart --help`; see
[`RUNNING_HART.md`](RUNNING_HART.md)).

## 4. Run the tests

```bash
export CTEST_OUTPUT_ON_FAILURE=1   # so a failure says why, not just that it failed
ctest -L short        # about a minute
ctest                 # the full suite
```

Without `CTEST_OUTPUT_ON_FAILURE`, `ctest` prints only pass or fail. With it, each
failure shows its one-line agreement summary — worst relative difference and worst
last digit — which is usually enough to classify the failure without re-running.
`make report` gives the same thing for every test, passing or not, in `tests.log`.

macOS shows tiny last-digit differences in a few tests. The comparison is
deliberately loose — relative difference ≤ 0.2%, or last printed digit within
2 — and counts those as passes. `docs/TONTO_BLESSING_TESTS.md` says what to do
about the few that fail anyway, and how to bless a reference yourself.

> **Do not use `gfortran-16` for debug builds.** It has two separate defects
> there: it miscompiles `-fcheck=bounds` (so the build drops the flag and you get
> no array bounds checking), and its debug build fails 34 of 71 short tests where
> gfortran-14 passes exactly. Release builds on 16 are fine. See
> [`TASK_GFORTRAN16_PORT.md`](TASK_GFORTRAN16_PORT.md).

## One macOS-specific oddity: one file compiled differently on Apple silicon

On Apple silicon (arm64), `shell1quartet.F90` is always compiled with
`-O2 -fno-schedule-insns`, because gfortran compiles that part of the two-electron
integral code wrongly at higher optimisation. `CMakeLists.txt` explains it where the
setting is made. There is nothing to do; it is mentioned so the unusual setting in the
build log is not a mystery.


## Other build types

The build type says what the build is for. It sets the optimisation, the
processor tuning and the BLAS library together, so it is the one choice to make.
Configure a separate directory for each type you keep.

| Type | For | What it sets |
|---|---|---|
| `release` | everyday work; the default | `-O3`, no processor tuning, the BLAS the system provides: Homebrew's OpenBLAS if installed, otherwise Accelerate |
| `fast` | the most speed on this machine | `-Ofast`, tuned for this processor, OpenBLAS (required). The last printed digits differ from `release`. |
| `reference` | results that match the stored test outputs; what CI builds | `-O3 -fno-fast-math`, no processor tuning, the netlib BLAS and LAPACK (required) |
| `debug` | finding a bug or a crash | no optimisation, array bounds checks, Tonto's internal checks |
| `release-static` | a self-contained binary to give to others | `release`, linked statically against the bundled LAPACK |

- **MPI goes with any of them:** add `-DMPI=1` (below).
- **Your own settings win.** `-DTONTO_ARCH_FLAG=...` (processor tuning) and
  `-DBLA_VENDOR=...` (BLAS) replace the type's choice, except in `reference`,
  which stops rather than build something that is not reproducible.
- Configure prints one line, `Build type ...`, saying what it chose.
- `reference` needs `brew install lapack`, and `fast` needs `brew install openblas`.

```bash
mkdir debug && cd debug
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=debug
make -j4
```

## Parallel (MPI) builds

```bash
cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_C_COMPILER=mpicc \
         -DCMAKE_BUILD_TYPE=release -DMPI=1
```

**The MPI must have been built with the same Fortran compiler as Tonto.** Tonto
does `USE mpi`, and Fortran `.mod` files are compiler-version specific.
Configure checks this and stops if they differ. `-DMPI=1` is a hard
requirement: if MPI is not found, configure fails rather than silently
producing a serial binary.

**The full recipe is in [`BUILDING_WITH_MPI.md`](BUILDING_WITH_MPI.md):** building an MPI
that matches, checking the build, running, and what to expect of the results.

A Homebrew Open MPI built with a different gcc will not work: check
`mpifort --version` against `gfortran-14`, and build a matching one if they differ.
The parallel build on macOS is built and checked by the *macOS-MPI* workflow.

---

## Where to go next

| | |
|---|---|
| Running Tonto | [`RUNNING_TONTO.md`](RUNNING_TONTO.md) |
| The `hart` program | [`RUNNING_HART.md`](RUNNING_HART.md) |
| Building and running with MPI | [`BUILDING_WITH_MPI.md`](BUILDING_WITH_MPI.md) |
| Source and executable layout | [`TONTO_LIBRARY_STRUCTURE.md`](TONTO_LIBRARY_STRUCTURE.md) |

> **Options are GNU long options.** Every Tonto program takes `--name` only —
> `tonto --input job.txt`, `hart --basis STO-3G`.
