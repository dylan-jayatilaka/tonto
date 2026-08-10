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
brew install gcc cmake openjdk python3 gnuplot
```

- `gcc` provides **`gfortran`**. Homebrew's `gcc` formula currently gives
  `gfortran-14`, which is the version this project standardises on.
- `openjdk` provides **`java`/`javac`** for the ANTLR4 `foo`→Fortran
  translator. If the build cannot find `javac`, add Homebrew's openjdk to your
  `PATH` as `brew` instructs — on Apple Silicon:
  ```bash
  echo 'export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"' >> ~/.zshrc
  ```
- **No BLAS or LAPACK to install** — macOS provides them through
  `Accelerate.framework`.
- **`gnuplot` is needed at *run* time, not build time**, to render the
  diagnostic plots a refinement writes. Without it the job still completes and
  the data files and gnuplot scripts are still written; you get a warning and
  no pictures.
- Optional parallel build: `brew install open-mpi` — see the compiler-matching
  rule below.

## 2. Get the source code

```bash
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto
git checkout release        # the tested branch — recommended
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
ctest -L short        # about a minute
ctest                 # the full suite
```

macOS shows tiny last-digit differences in a few tests. The comparison is
deliberately loose — relative difference ≤ 0.2%, or last printed digit within
2 — and counts those as passes.

## One macOS-specific oddity: the arm64 compiler pin

**arm64.** `shell1quartet.F90` is pinned to `-O2 -fno-schedule-insns` on arm64
macOS, working around a gfortran miscompilation of the two-electron integral
code. `CMakeLists.txt` explains it at the pin. Nothing to do; it is mentioned
so the odd flag in the build log is not a mystery.


## Other build types

The build type is the one real choice. Configure a separate directory for each
type you keep.

| Type | For |
|---|---|
| `release` | Optimised and tested; what CI runs and what the reference outputs were blessed with. Use this unless you have a reason not to. |
| `debug` | `-O0`, runtime checks, error messages. For diagnosing a crash. |
| `fast` | Aggressive optimisation. Faster, may perturb the last printed digits. |
| `release-static` | A self-contained binary for redistribution. Larger. |

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

**Validate parallel results before trusting them.**
[`TONTO_AND_MPI.md`](TONTO_AND_MPI.md) records what a parallel run does and does
not reproduce.

**Untested on macOS.** A Homebrew Open MPI built against a different gcc will
not work, so expect to check `mpifort --version` against `gfortran-14`.

---

## Where to go next

| | |
|---|---|
| Running Tonto | [`RUNNING_TONTO.md`](RUNNING_TONTO.md) |
| The `hart` program | [`RUNNING_HART.md`](RUNNING_HART.md) |
| What a parallel build does and does not reproduce | [`TONTO_AND_MPI.md`](TONTO_AND_MPI.md) |
| Source and executable layout | [`TONTO_LIBRARY_STRUCTURE.md`](TONTO_LIBRARY_STRUCTURE.md) |

> **Options are GNU long options.** Every Tonto program takes `--name` only —
> `tonto --input job.txt`, `hart --basis STO-3G`.
