# Building and running Tonto with MPI

A parallel Tonto is the same source built with an MPI compiler wrapper and `-DMPI=1`. This page
is the whole recipe: the one rule that causes most failed builds, the build, how to check the
result, and how to run. The platform pages ([Linux](BUILDING_ON_LINUX.md),
[macOS](BUILDING_ON_MACOS.md), [Windows](BUILDING_ON_WINDOWS.md)) cover everything that comes
before it.

## 1. The rule: your MPI must be built with your Fortran compiler

Tonto does `USE mpi`, and Fortran `.mod` files can be read only by the compiler version that wrote
them. An MPI package built with a different gfortran cannot be used:

```
Fatal Error: Cannot read module file '.../mpi.mod' opened at (1),
because it was created by a different version of GNU Fortran
```

Setting `OMPI_FC` does not help: it changes the compiler the wrapper calls, not the one that
built the modules. Configure checks for this and stops with a message naming both compilers.

Check what you have:

```bash
mpifort --version     # must report the same gfortran as gfortran-14 --version
```

Ubuntu's `libopenmpi-dev` and Homebrew's `open-mpi` are usually built with a different gcc than
`gfortran-14`. If so, build an Open MPI that matches. It sits beside the system copy and is
selected by path.

On Linux:

```bash
curl -O https://download.open-mpi.org/release/open-mpi/v5.0/openmpi-5.0.9.tar.bz2
tar xf openmpi-5.0.9.tar.bz2 && cd openmpi-5.0.9
./configure --prefix=$HOME/opt/openmpi-gf14 FC=gfortran-14 CC=gcc-14
make -j4 && make install
```

On macOS with Homebrew, three more options are needed, or configure stops with *"Either
libevent or libev support is required"*:

```bash
brew install libevent hwloc
./configure --prefix=$HOME/opt/openmpi-gf14 \
            FC=gfortran-14 CC=clang CXX=clang++ --disable-mpi-cxx \
            --with-libevent=$(brew --prefix libevent) \
            --with-hwloc=$(brew --prefix hwloc) --with-pmix=internal
make -j4 && make install
```

## 2. Build

From the top of the source tree:

```bash
OMPI=$HOME/opt/openmpi-gf14        # or wherever your matching MPI is
cmake -S . -B build-mpi \
      -DCMAKE_Fortran_COMPILER=$OMPI/bin/mpifort \
      -DCMAKE_C_COMPILER=$OMPI/bin/mpicc \
      -DMPI=1 -DCMAKE_BUILD_TYPE=release
cmake --build build-mpi -- -j4
```

- **`-DMPI=1` is a hard requirement.** If MPI is not found, configure fails; it does not build
  a serial program instead.
- **MPI goes with any build type.** `-DMPI=1` with `-DCMAKE_BUILD_TYPE=debug` gives a parallel
  debug build. Use `reference` to compare against the stored test outputs.
- A C++ compiler is not needed and `-DCMAKE_CXX_COMPILER` is ignored.

Confirm the program is parallel:

```bash
ldd build-mpi/tonto | grep mpi         # otool -L on macOS
```

On a cluster, load the compiler and MPI modules first and give CMake the site's Fortran wrapper
with `-DCMAKE_Fortran_COMPILER=`. The three settings that matter are the compiler, the build type
and `-DMPI=1`.

## 3. Check it

Run the π check first. It integrates π in parallel on 1, 2 and 4 processes and must give the same
answer each time; it needs no stored output.

```bash
ctest --test-dir build-mpi -L mpi
sh scripts/check_mpi_pi.sh build-mpi/run_mpi_pi $OMPI/bin/mpirun 1 2 4
```

Wrong at every process count means the integration or the sum is broken. Right on one process
and wrong on two or four means the processes' partial sums are not being combined.

Then the test suite under the launcher:

```bash
python3 scripts/suite_report.py --build-dir build-mpi --suites short \
        --mpi --mpi-ranks 4 --mpi-launcher $OMPI/bin/mpirun
```

The launcher must come from the same MPI the program was linked with.

## 4. Run

```bash
$OMPI/bin/mpirun -n 4 build-mpi/tonto
```

in a directory holding `stdin`, as for a serial run ([`RUNNING_TONTO.md`](RUNNING_TONTO.md)).

## 5. What to expect of the results

- **A parallel build differs from a serial one even on one process.** Sums over a parallel loop
  are added in a different order, and the build turns off the `PURE` and `ELEMENTAL` attributes
  throughout, which changes how the compiler optimises. Expect differences in the last printed
  digits.
- **Results depend on the number of processes** in those last digits, for the same reason: each
  process takes every $`P`$-th pass of a loop, for $`P`$ processes, and the partial sums are added
  at the end.
- **Check a parallel result against a serial one before relying on it.** The parallel build has
  known faults, and the test suite under MPI does not give the same count from run to run. They
  are listed in [`TONTO_KNOWN_ISSUES.md`](TONTO_KNOWN_ISSUES.md).
