# Welcome to Tonto!
[![Build Status](https://travis-ci.org/dylan-jayatilaka/tonto.svg?branch=master)](https://travis-ci.org/dylan-jayatilaka/tonto)

## 1. Get ready ...

* For running jobs, we recommend downloading the latest release for your platform (see "releases" tab above)
* If you want to develop, first install `git` and  follow the compile instructions below.

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
* `openmp-3.0` or `mpich2` (for parallel)
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
command for your fortran compiler. We recommend `gfortran`.

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
   cmake .. -DCMAKE_BUILD_TYPE=debug
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

## 4. Developing & Ettiquette

Developers are very welcome!

Just contact Dylan, and you will be added to the list so you can push back your changes.

Of course, you will have to know something about programming.
* I hope you know how to use `git`?
* We use the `foo` preprocessor which converts to Fortran95, so knowing something about modern Fortran is essential.
* There is a description of `foo` on some web pages, but probably it is easier to look at the code.
* Also, we use an object oriented style; each file represents an abstract data type or class.
* All objects are "friends" and the derived types are in the `types.foo` file.

Finally, there are the rules, which are brief, and which follow.
* In the beginning do your work in a branch.
* Your work must not break any tests. This code is used in at least two commercial projects!
* When you compile under `-DCMAKE_BUILD_TYPE=debug` there must be no warnings, as far as possible.
* Methods should have meaningful names, and meaningful comments, explaining at least the input arguments, or what state changes happen to the `self` object.
* There must be no unused routine arguments and no unused variables, as far as possible.
* Avoid using capitals for variables because capitals are macros and used for types. Single letter capitals are OK.
* Use the .create / .destroy / .created / .destroyed / .allocated / .desllocated / .associated / .disaasociated methods where possible so I can migrate to a different language at a later date.
* Use the standard 3 space indent for Fortran, especially for routine methods, so vim collapsing works. Plus, it looks better.
* Try to keep method code short, to a screenful if possible --- use helper routines!
* If must keep dead code, alternative algorithms, keep it neat and make a main `algorithm` which may call label `algorthm_v1`, `algorithm_v2`, ... so that we can learn what works and what doesn't.
* If you satisy all the above, you should push to the main branch!

That's all.

## Problems, bugs, contributions

Let me know at

```
   dylan.jayatilaka@gmail.com
```

# How to run tonto

See [the wiki](https://github.com/dylan-jayatilaka/tonto/wiki/How-to-run-tonto) for details.
