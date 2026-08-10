# Tonto

|  | release | debug | MPI |
|---|---|---|---|
| **Linux** | [![Linux-release](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml?query=branch%3Arelease) | [![Linux-debug](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-debug.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-debug.yml?query=branch%3Arelease) | [![Linux-MPI](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-mpi.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-mpi.yml?query=branch%3Arelease) |
| **Windows/WSL** | [![WSL-release](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl.yml?query=branch%3Arelease) | [![WSL-debug](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl-debug.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl-debug.yml?query=branch%3Arelease) | — |
| **macOS** | *none yet* | *none yet* | — |

Tonto is a quantum chemistry and crystallography package, with a focus on X-ray
and electron structure refinement — especially **Hirshfeld atom refinement**,
**X-ray structure factor calculation**, and **X-ray wavefunction refinement**.

The scientific code is written in **Foo**, an object-oriented preprocessor
language translated to modern Fortran. *Foo* is `Fortran` reversed, for object
oriented Fortran: identical to Fortran at the expression level, different in how
variables, subroutines and functions are declared.

---

## Compile quickstart, on Ubuntu/Debian Linux

For another platform, jump straight to its page —
[**Linux**](docs/BUILDING_ON_LINUX.md),
[**macOS**](docs/BUILDING_ON_MACOS.md),
[**Windows/WSL**](docs/BUILDING_ON_WINDOWS.md) — or see
[**`docs/BUILDING_TONTO.md`**](docs/BUILDING_TONTO.md) for build types, MPI and
clusters.

```
sudo apt install make default-jdk gfortran-14 libblas-dev liblapack-dev python3 gnuplot git

git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto && git checkout release

mkdir build && cd build
cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
make -j4
```

> **`gnuplot` 6.0+ is needed at run time**, not just to build: Tonto invokes it
> to draw the diagnostic plots at the end of a refinement. Without it the job
> still finishes and the plot data and scripts are still written — you just have
> to run `gnuplot` yourself to see the pictures.

That gives you **`build/tonto`** and **`build/hart`**. Then check it works:

```
make report        # runs the suites, writes a grouped agreement table
```

Two correct builds can differ in the last printed digits (compiler, BLAS, grid
ordering), so `make report` grades every test three ways — **exact**, **loose**
(within 0.2 % *or* ±2 in the last digit), and **last-digit**. **Only `loose`
decides pass/fail.** A bare `ctest` shows pseudo-failures for this reason.

> **`gfortran-14`, not the distro default.** Ubuntu 24.04's plain `gfortran` is
> version 13. This project standardises on 14 — it is what CI builds with, what
> the reference outputs in `tests/` were blessed with, and what any MPI you use
> must also have been built with, since Tonto does `USE mpi` and Fortran `.mod`
> files are compiler-version specific.

> **Options are GNU long options** — `tonto --input job.txt`,
> `hart --basis STO-3G`. The single-dash spellings were removed.

## Parallel (MPI) builds, on every platform that has an MPI

Parallel is a **build option, not a platform**: the same two flags apply
wherever an MPI library is installed.

```
cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_C_COMPILER=mpicc \
         -DCMAKE_BUILD_TYPE=release -DMPI=1
```

**The MPI must have been built with the same Fortran compiler as Tonto.** Tonto
does `USE mpi`, and Fortran `.mod` files are compiler-version specific.
Configure checks this and stops if they differ. `-DMPI=1` is a hard
requirement: if MPI is not found, configure fails rather than silently
producing a serial binary.

How far each platform has been taken:

| Platform | Parallel build |
|---|---|
| **Linux** | Tested, and in CI — the Linux-MPI badge above |
| **Windows/WSL** | Untested. WSL is Ubuntu, so `apt install openmpi-bin libopenmpi-dev` and the recipe above should apply unchanged |
| **macOS** | Untested. `brew install open-mpi`, subject to the compiler-matching rule above |

**Validate parallel results before trusting them.**
[`docs/TONTO_AND_MPI.md`](docs/TONTO_AND_MPI.md) records what a parallel run
does and does not reproduce, and the defects found so far.

## Documentation overview

Everything lives in this repository, versioned with the code it describes.

| | |
|---|---|
| [**`docs/BUILDING_ON_LINUX.md`**](docs/BUILDING_ON_LINUX.md) | building on Linux — start here if you are on Ubuntu/Debian |
| [**`docs/BUILDING_ON_MACOS.md`**](docs/BUILDING_ON_MACOS.md) | building on macOS, via Homebrew |
| [**`docs/BUILDING_TONTO.md`**](docs/BUILDING_TONTO.md) | build types, MPI and clusters — common to every platform |
| [**`docs/RUNNING_TONTO.md`**](docs/RUNNING_TONTO.md) | running Tonto, input/output conventions, practical set-up |
| [**`docs/RUNNING_HART.md`**](docs/RUNNING_HART.md) | the `hart` program — options, testing, fragHAR |
| [**`docs/RUNNING_RGBI.md`**](docs/RUNNING_RGBI.md) | the `rgbi` program — options, the picture pipeline, testing |
| [**`docs/INSTALLING_RGBI.md`**](docs/INSTALLING_RGBI.md) | installing the RGBI picture tools (for workshop participants) |
| [**`docs/TONTO_LIBRARY_STRUCTURE.md`**](docs/TONTO_LIBRARY_STRUCTURE.md) | what lives where, and the module structure |
| [**`docs/TONTO_DEVELOPER.md`**](docs/TONTO_DEVELOPER.md) | developer reference, including **writing parallel (MPI) code in Foo** |
| [**`docs/FOO_GRAMMAR_DOCUMENTATION.md`**](docs/FOO_GRAMMAR_DOCUMENTATION.md) | the Foo language and its translation to Fortran |
| [**`docs/TONTO_AND_MPI.md`**](docs/TONTO_AND_MPI.md) | the parallel build, its numeric characterisation, and the defect register |
| [**`docs/BUILDING_ON_WINDOWS.md`**](docs/BUILDING_ON_WINDOWS.md) | building on Windows via WSL — and the four WSL traps and how they are guarded |
| [**`docs/TONTO_CONTINUOUS_INTEGRATION.md`**](docs/TONTO_CONTINUOUS_INTEGRATION.md) | what each workflow runs, and how to read a result |
| [**`docs/MAKING_CALL_GRAPHS.md`**](docs/MAKING_CALL_GRAPHS.md) | call/use graphs and dead-code elimination |
| [**`docs/EDITING_TONTO_WITH_VIM.md`**](docs/EDITING_TONTO_WITH_VIM.md) | vim set-up — tags, folding, completion |
| [**`DEFERRED.md`**](DEFERRED.md) | known issues and deferred work, with the reasoning |

## The CI badges, and what each one actually tests

They track the **`release`** branch — the one the quickstart clones. They are not
all the same kind of badge:

- **CI (Linux-release)** — the gate. This one must be green.
- **CI (Linux-debug)** — carries four longstanding `-O0` floating-point and
  structural failures that are not code defects (see `DEFERRED.md`), so its
  suite step is informational.
- **CI (Linux-MPI)** — not on every push. Ubuntu's Open MPI is built against
  gcc-13 while this project standardises on gfortran-14, and `USE mpi` makes
  `.mod` files compiler-version specific, so the workflow builds Open MPI from
  source; it is therefore cached, scheduled weekly, and triggered only by
  MPI-relevant paths. Its gate is the π rank-invariance check
  (`scripts/check_mpi_pi.sh`), not the suite. A red MPI badge means the build
  broke, or π stopped being rank-count independent — both real.
- **CI (WSL-release)** — builds and tests inside a real WSL2 Ubuntu on a Windows
  runner, because WSL looks enough like Linux that the ordinary build "works"
  right up until it does not. Every push, plus weekly.
- **CI (WSL-debug)** — the WSL counterpart of CI (Linux-debug), deliberately
  narrow: it builds `debug` and runs two fast jobs to prove the binary executes.
  It does not run the short suite, for the same reason as CI (Linux-debug).
  Weekly, a day after WSL-release, so two hour-long Windows jobs never queue
  against each other.

The empty **macOS** row is deliberate, and is the point of the table: macOS
runners are free for this public repository, and a macOS job is the only thing
that can guard the two arm64 compiler pins whose failure mode is wrong numbers
rather than crashes. See the high-priority item in [`DEFERRED.md`](DEFERRED.md).

Two workflows are not build types and sit outside the table:
[![RGBI toolchain](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-rgbi.yml/badge.svg?branch=antlr4)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-rgbi.yml)
proves the RGBI picture-tool install list from a bare `ubuntu:24.04`, and
`ci-rgbi-macos.yml` probes the same list on a real Mac weekly (deliberately
unbadged: macOS is not supported yet, so it must not read as if it were).

## Getting help, reporting bugs, and contributing

Email **dylan.jayatilaka@gmail.com** (I am slow to reply — you may have better
luck via people who know me).
