# Welcome to Tonto!
[![Build Status](https://travis-ci.org/dylan-jayatilaka/tonto.svg?branch=master)](https://travis-ci.org/dylan-jayatilaka/tonto)

## 1. Get ready ...

* If you're not planning on modifying tonto source (i.e. developing), we recommend 
downloading the latest release for your platform.
* If you want to develop, first install `git` and 
  and follow the compile instructions below.

### On Linux

First, open a terminal and clone the repository:

```
   git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
```

While waiting, in another terminal window, or using your
software package manager, install:

* `perl`
* `gfortran`
* `make`
* `blas` 
* `lapack` 
* `openmpi-3.0` (for parallel)
* `python3` (recommended for testing)
* `gnuplot` (recommended)

### On MacOS

See [Building on MacOS](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-MacOS)

### On Windows

See [Building on Windows](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-Windows)

## 2. Get set ...

To compile Tonto, first enter the `tonto` directory downloaded with
`git` :

```
    cd tonto
```

Then make a `build` directory (name is up to you) and enter that :

```
    mkdir build && cd build
```

Use cmake to generate the build (default uses Makefiles), and compile the programs :

```
    cmake ..
    make -j
```

If you want a specific compiler, use :

```
   cmake .. -DCMAKE_Fortran_COMPILER=<insert-your-compiler-here>
   make -j
```

where you should replace <insert-your-compiler-here> with the
command for your fortran compiler. We recommend `gfortran-6`.

If you want a static executable for redistribution set the build type
to RELEASE-STATIC as follows:

```
   cmake .. -DCMAKE_BUILD_TYPE=RELEASE-STATIC
   make -j
```

To make an MPI parallel version (e.g. using openmpi) , type :

```
   cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_CXX_COMPILER=mpicxx -DCMAKE_C_COMPILER=mpicc -DMPI=1
   make -j
```

To change build type (e.g. make a DEBUG version) use this option :

```
   cmake .. -DCMAKE_BUILD_TYPE=Debug
   make -j
```
In the case you do NOT have lapack and blas installed, there is a packaged lapack included in tonto, which you can also request manually:

```
   cmake .. -DCOMPILE_LAPACK=ON
   make -j
```

By default the `tonto` program is built with Release flags.
The executable program is located at:

```
    build/tonto(.exe)
```

The standalone Hirshfeld atom refinement terminal
(HARt) program will be located at:

```
   build/hart(.exe)
```

Copy the program `build/hart` anywhere you like 
For help type `hart -help`.

## 3. Go!

The tests use the `test.py` script located in `scripts` 
to check the difference between outputs.  This should
defer to `sbftool` for SBF formatted files, and will
overcome small numerical differences.

To run all tests, in the build directory type:
```
   ctest
```
Or you may use `ctest` directly and run only tests matching
certain labels or regular expressions; or specify the number 
of processors to use when running tests :
```
   ctest -L short    # this will run all tests with the label short.
   ctest -R h2o      # this will run all tests with h2o in their name.
   ctest -L long -j4 # this will run all long tests with 4 jobs at a time.
```

You should get mostly the `passed` message --- but there may be small
numerical differences which lead to pseudo-failures. If you are keen
you may check for *true* failed tests.

To check failures go into the `tests/` folder and then from there into the
folder with the same name as the job that failed. You should see there
pairs of files called `<file>` and `<file>.bad`. You have to compare
the reference `<file>` and alleged failed output file `<file.bad>`
using your favourite tool e.g.

```
   vimdiff stdout stdout.bad
```

## Problems, bugs, contributions

Let me know at

```
   dylan.jayatilaka@gmail.com
```

# How to run tonto

See [the wiki](https://github.com/dylan-jayatilaka/tonto/wiki/How-to-run-tonto) for details.
