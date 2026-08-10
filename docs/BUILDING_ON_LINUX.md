# Building Tonto on Linux

Ubuntu/Debian, the assumed and best-supported platform. Everything here is one
pass from a clean machine to a tested binary. Other platforms: [macOS](BUILDING_ON_MACOS.md),
[Windows/WSL](BUILDING_ON_WINDOWS.md).

---

## 1. Install the prerequisites

```bash
sudo apt install make cmake default-jdk gfortran-14 libblas-dev liblapack-dev \
                 python3 gnuplot git
```

- **`gfortran-14`, not the distro default `gfortran`.** Ubuntu 24.04's plain
  `gfortran` is version 13. The project standardises on **14**: that is what CI
  builds with, what the reference outputs in `tests/` were blessed with, and —
  because Tonto does `USE mpi` and Fortran `.mod` files are compiler-version
  specific — what any MPI you use must also have been built with. Version 13
  will generally compile, but do not report a numeric difference against the
  references without first checking on 14.
- `default-jdk` provides `java`/`javac` for the ANTLR4 `foo`→Fortran translator.
  The ANTLR jar itself is downloaded automatically on the first `cmake` run
  (internet needed for that one configure).
- **`gnuplot` (6.0 or later) is needed at *run* time, not build time.** Tonto
  runs it for you: at the end of a refinement it writes each diagnostic plot as
  a data file *and* a gnuplot script, then invokes `gnuplot` to render a `.png`.
  Without it the job still completes and both the data and the scripts are
  still written; you get a warning naming the command to run by hand, and no
  pictures.
- `python3` runs the test harness.
- Optional: `graphviz` for the developer call-graphs;
  `openmpi-bin libopenmpi-dev` for a parallel build.

## 2. Get the source code

```bash
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto
git checkout release        # the tested branch — recommended
```

`--recursive` pulls the submodules. To keep several branches side by side,
clone into a named folder: `git clone --recursive … tonto-release`.

## 3. Configure and build

Tonto builds **out of source**: make a build directory, configure it once, then
`make`.

```bash
mkdir build && cd build
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
make -j4
```

> **About `-j`.** Translation runs one JVM per `.foo` file, which is
> memory-heavy. A bare `make -j` (unbounded) can thrash the machine — cap it:
> `make -j4 -l8` (at most 4 jobs, pause while load > 8). Lower `-j` first if a
> build stalls.

You now have **`build/tonto`** (the main program) and **`build/hart`**
(standalone Hirshfeld atom refinement — `hart --help`; see
[`RUNNING_HART.md`](RUNNING_HART.md)).

## 4. Run the tests

```bash
ctest -L short        # about a minute
ctest                 # the full suite
```

The comparison is deliberately loose — relative difference ≤ 0.2%, or last
printed digit within 2 — because the references were blessed on one compiler
and one machine.


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

Install it with `sudo apt install openmpi-bin libopenmpi-dev`. Ubuntu's package
is built against a different gcc than `gfortran-14`, so if configure rejects it,
build one to match:

```bash
./configure --prefix=$HOME/opt/openmpi-gf14 FC=gfortran-14 CC=gcc-14
```

and put its `bin` first on `PATH`. This is the platform where the parallel build
is tested and in CI.

## On a cluster

Environments vary too much to script. Load your compiler and MPI modules first,
then use the recipe above, overriding the compiler if needed with
`-DCMAKE_Fortran_COMPILER=<your ftn wrapper>`. The three knobs that matter are
the compiler, the build type and `-DMPI=1`.

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
