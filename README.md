# Welcome to Tonto!

# Erice 2025 workshop people!

If you are a workshop attendee, the lab folder and instructions are [here](https://drive.google.com/drive/folders/17OWncmSsFbKAlW8mZb9EKzJuW0GAjykG).

[![Build Status](https://travis-ci.org/dylan-jayatilaka/tonto.svg?branch=master)](https://travis-ci.org/dylan-jayatilaka/tonto)

## 0. For developers: How to push with a new token

To set up your local git repo to push to github, use the following

```
git remote set-url origin https://USERNAME:TOKEN@github.com/USERNAME/REPO.git
```

Replace USERNAME with your own github user name.

You can get a classic TOKEN from :

Settings photo-> Settings -> develepor-setting -> personal-access-token -> tokens (classic) -> Generate new tokens (classic).

You can get to this location more easily by going to the location below:

```
https://github.com/settings/tokens
```

The selections are quite hard to find: at the left, bottom, or top right of the menus.

## 1. Get ready ...

First install `git` and and follow the compile instructions below.

### On Linux

First, open a terminal and clone the repository:

```
   git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
```

While waiting, in another terminal window, or using your software package manager, install:

* `perl`
* `gfortran`
* `make`
* `blas` 
* `lapack` 
* `python3`
* `gnuplot` (recommended)

Many of these programs like python may already be installed.

If you are going to compile a parallel version of the program also install openmpi and friends:

```
sudo apt install openmpi-bin openmpi-common openssh-client openssh-server libopenmpi-dev
```

### On MacOS

See [Building on MacOS](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-MacOS)

### On Windows

See [Building on Windows](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-Windows)

## 2. Get set ...

To compile Tonto, first enter the `tonto` directory downloaded with `git` :

```
    cd tonto
```

Next checkout the release branch

```
   git checkout release
```

Now make a `build` directory (name is up to you) and enter that :

```
    mkdir build && cd build
```

Now set your compiler and start compiling :

```
   cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=fast
   make -j
```
Then you are done.

You can, and I recommend, to make a `debug` version which prints error messages. In the `tonto` folder type :

```
    mkdir debug
    cd debug
    cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=debug
    make -j

```

If you want a static executable for redistribution set the build type to `RELEASE-STATIC` as follows:

```
   mkdir static
   cd static
   cmake .. -DCMAKE_BUILD_TYPE=release-static
   make -j
```
  
If you want version with no instrumentation and no error checking, which is the fastest, then do:
  
```
   mkdir static
   cd static
   cmake .. -DCMAKE_BUILD_TYPE=release-static -DNO_ERROR_MANAGEMENT
   make -j
```

By default the `tonto` program is built with the `RELEASE` flags i.e. not static. The static verssion is a lot larger in size.
  
To make an MPI parallel version (e.g. using openmpi) type :

```
   mkdir mpi
   cd mpi
   cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_CXX_COMPILER=mpicxx -DCMAKE_C_COMPILER=mpicc -DCMAKE_BUILD_TYPE=fast -DMPI=1
   make -j
```

Consider also using `-DNO_ERROR_MANAGEMENT` in this case for even more speed.

## 3. Where is the program?

The executable program is located at:

```
    build/tonto(.exe)
```

The standalone Hirshfeld atom refinement terminal (`hart`) program will be located at:

```
   build/hart(.exe)
```

Copy the program `build/hart` anywhere you like  For help type `hart -help`.

## 4. Run tests

To run all tests, in the build directory type:

```
   ctest
```

You should get mostly the `passed` message, but there may be small numerical differences which lead to pseudo-failures. If you are keen you may check for *true* failed tests.

Actually, its better to save the tests resukts to a file:

```
   ctest >& tests.log &
```

Then you can review the results later at your leisure. 
  
Here is a nice thing for problem tests: you may use `ctest` directly and run only tests matching certain labels or regular expressions; or specify the number  of processors to use when running tests :

```
   ctest -L short    # this will run all tests with the label short.
   ctest -R h2o      # this will run all tests with h2o in their name.
   ctest -L long -j4 # this will run all long tests with 4 jobs at a time.
```

To check failures go into the `tests/` folder and then from there into the folder with the same name as the job that failed. You should see there pairs of files called `<file>` and `<file>.bad`.

You must compare the reference `<file>` and alleged failed output file `<file.bad>` using your favourite tool e.g.

```
   vimdiff stdout stdout.bad
```

## Problems, bugs, contributions

Let me know at
```
   dylan.jayatilaka@gmail.com
```
I am not good at responding. Best to contact some people that know me. If you google you might find such people. There aren't many, as I'm a misanthrope. 

# How to run tonto

See [the wiki](https://github.com/dylan-jayatilaka/tonto/wiki/How-to-run-tonto) for details.
