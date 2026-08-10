# Tonto

|  | release build | debug build | parallel (MPI) build |
|---|---|---|---|
| **Linux** | [![Linux-release](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml?query=branch%3Arelease) | [![Linux-debug](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-debug.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-debug.yml?query=branch%3Arelease) | [![Linux-MPI](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-mpi.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-mpi.yml?query=branch%3Arelease) |
| **Windows/WSL** | [![WSL-release](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl.yml?query=branch%3Arelease) | [![WSL-debug](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl-debug.yml/badge.svg?branch=release)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci-wsl-debug.yml?query=branch%3Arelease) | — |
| **macOS** | *none yet* | *none yet* | — |

Each badge builds Tonto on that platform and runs a test suite, on the
`release` branch. What each one covers is in
[**Continuous integration**](docs/TONTO_CONTINUOUS_INTEGRATION.md).

Tonto is a quantum chemistry and crystallography package, with a focus on X-ray
and electron structure refinement — especially **Hirshfeld atom refinement**,
**X-ray structure factor calculation**, and **X-ray wavefunction refinement**.

The scientific code is written in **Foo**, an object-oriented preprocessor
language translated to modern Fortran. *Foo* is `Fortran` reversed, for object
oriented Fortran: identical to Fortran at the expression level, different in how
variables, subroutines and functions are declared.

## Get it

Ready-built programs for Linux and Windows, needing no compiler, are on the
[**releases page**](https://github.com/dylan-jayatilaka/tonto/releases).

To build from source, clone the repository — **`--recursive` matters**, the
submodules are not optional:

```
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto
```

then follow the page for your platform. Each is self-contained.

| | |
|---|---|
| [**Linux**](docs/BUILDING_ON_LINUX.md) | Ubuntu/Debian |
| [**macOS**](docs/BUILDING_ON_MACOS.md) | via Homebrew |
| [**Windows**](docs/BUILDING_ON_WINDOWS.md) | via WSL2 |

## Learn it

The [**workshop**](workshop/WORKSHOP.md) is three worked exercises: a Hirshfeld
atom refinement on ammonia, a refinement on urea followed by a bond-index
analysis, and an X-ray constrained wavefunction fit.

## Everything else

[**Documentation**](docs/DOCUMENTATION.md) — running the programs, the Foo
language, the parallel build, and the developer references.

## Getting help, reporting bugs, and contributing

Email **dylan.jayatilaka@gmail.com** (I am slow to reply — you may have better
luck via people who know me).
