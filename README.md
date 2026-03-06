# Welcome to Tonto!

[![Build Status](https://travis-ci.org/dylan-jayatilaka/tonto.svg?branch=master)](https://travis-ci.org/dylan-jayatilaka/tonto)

# Erice 2025 workshop people!

If you are a workshop attendee, the lab folder and instructions are [here](https://drive.google.com/drive/folders/17OWncmSsFbKAlW8mZb9EKzJuW0GAjykG).

# Developers: How to push with a new token

To set up your local git repo to push to github, use the following

```
git remote set-url origin https://USERNAME:TOKEN@github.com/USERNAME/REPO.git
```

Replace USERNAME with your own github user name.

You can get a classic TOKEN from :

Settings photo-> Settings -> developer-setting -> personal-access-token -> tokens (classic) -> Generate new tokens (classic).

These selections are quite convoluted: at the left, bottom, or top right of the menus.

You can go directly to this location:

```
https://github.com/settings/tokens
```


# Getting started and Compiling

You will need to be on the super user to install software on your machine.

## 0. Operating systems

### On Linux

I assume you are using the latest Ubuntu (I use it).

I also assume you are using the command line in a bash terminal - the default if you click the terminal icon.

### On MacOS

Tonto has many failures on the Apple M2 with Tahoe 26.3. It is not recommended.

If you wish to proceed, the strategy is to use homebrew to install GNU tools, then follow the Linux instructions below.

The instructions below may be helpful, but they are out of date.

See [Building on MacOS](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-MacOS)

### On Windows

Tonto has not been tested on Windows recently.

The strategy is as for the Mac - install GNU tools and compile.

The instructions below are out of date since they predate WSL.

See [Building on Windows](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-Windows)

## 1. Install `git` and clone the repo

You will need to be on the super user to install software on your machine.

First install `git` 

```
   sudo apt install git
```

Next, open a terminal and clone the repository:

```
   git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
```

To compile a particular branch e.g. the latest `release` branch (which is recommended) clone into a named folder, then `switch` to the branch you want:

```
   git clone --recursive https://github.com/dylan-jayatilaka/tonto.git tonto-release
   cd tonto-release
   git switch release
```
Unless you know what you are doing you should only use the default `master` branch or the `release` branches.

## 2. Install other software

Make sure you have all the other needed software installed, specifically: `make`, `perl` (for the `foo` language), `gfortran`, `blas`, `lapack`, `python3` (for testing), and `gnuplot` (for graphs and plots). In a new terminal type:

```
   sudo apt install make perl gfortran gcc g++ libblas-dev liblapack-dev python3 gnuplot ctags
```

To compile a parallel version of the program via openmpi and friends type:

```
   sudo apt install openmpi-bin openmpi-common openssh-client openssh-server libopenmpi-dev 
```
This should install the `mpifort` parallel fortran compiler. Installing all the components of a parallel compiler can be tricky, especially in a supercomputer environment. Usually it is required to load the the correct modules, via a `module load` command. In addition, you may need to run your program in a queue. For this reason, it is best to compile and test a single processor version first, before proceeding to a full parallel version.

## 3. Set up a build folder and go into it

Inside the tonto folder, make a build directory (the name is up to you) and enter it. This is where the program is compiled and built. I will call it `release` since it will hold the standard, released version of the programs. Type:

```
   mkdir release
   cd release
```

I recommended to compile (or build) three other versions tonto: `debug`, `fast` and `mpi` parallel versions.

You must make separate folders for each build version.


## 4. Choose a compiler, and compile executables

You may use different compilers but we use the latest `gfortran` and `gcc` compilers. 

For the release version, in your build folder, type:

```
   cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=release
   make -j
```
The screen will produce a lot of output. When it finished you are done.

To make a `fast` version, in your `fast` build folder, type:

```
    cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=fast
    make -j

```
The `fast` version is, of course, faster. However this may come at the expense of bugs, and numerical imprecision. Please ensure you always run the tests (see below) to ensure the code is working properly.

To make a `debug` version, in your `debug` build folder, type:

```
    cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=debug
    make -j

```
The `debug` version runs slower, but it has more error messages, and it gives a useful traceback if the program crashes, and it can be used in the debugger. This can help for understanding problems. The `debug` version is essential for developers.

If you want a static executable, for redistribution, set the build type to `RELEASE-STATIC` as follows:

```
   cmake .. -DCMAKE_Fortran_COMPILER=gfortran DCMAKE_BUILD_TYPE=release-static
   make -j
```
The static version is a lot larger in size.
  
For production i.e. large calculations it is recommended to make an MPI parallel version. However you absolutely must test the parallel version to check that the results are correct (see below) before using it. To make a MPI parallel version, in your MPI build folder, type:
```
   cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_CXX_COMPILER=mpicxx -DCMAKE_C_COMPILER=mpicc -DCMAKE_BUILD_TYPE=fast -DMPI=1
   make -j
```
Consider also using `-DNO_ERROR_MANAGEMENT` which will remove any error checking code and will make the program even faster.

## 5. Where are the compiled programs?

The executable program is located in the build folde e.g.

```
    release/tonto
```

The standalone Hirshfeld atom refinement terminal (`hart`) program will be located at:

```
   release/hart
```
Copy the program `build/hart` anywhere you like  For help type `hart -help`.

There are other test programs which are made and used.

## 6. Where is the code?

All of the programs are in the `runfiles/` folder.

The source code used by the programs is in the `foofiles/` folder. It code is written in the `foo` language, which I invented. `Foo` is a bit like `Julia`, but was based on `Eiffel` and `Sather`.

`Foo` code is arranged into modules, which are like classes. The name of each class is the same as the name of the head part of each file in `foofiles/`. Some modules are large, and are split into submodules. The `foo` compiler is a source-to-source converter: `foo` source code is currently translated into `fortran` 2008. The generated `fortran` code may have more, or less, error checking, or additions for parallel execution. The generated `fortran` source also resides in the build folder, along with the object files and the executable programs. 



## 7. Run tests please!

To run all tests, in the build directory type:

```
   ctest
```

You should get mostly the `passed` messages, but there may be small numerical differences which lead to pseudo-failures. You should check for *true* failed tests. To do that, it is better to save the tests results to a file:

```
   ctest >& tests.log &
```

Then you can review the results later at your leisure. To check failures go into the `tests/` folder and then from there into the folder with the same name as the job that failed. You should see there pairs of files called `<file>` and `<file>.bad`. You must compare the reference `<file>` and alleged failed output file `<file.bad>` using your favourite tool e.g.

```
   vimdiff stdout.bad stdout
```

Here is a nice thing for problem tests: you may use `ctest` in the `build/` folder and run only tests matching certain labels or regular expressions for the jobs that failed; or specify the number of processors to use when running tests :

```
   ctest -L short    # this will run all tests with the label short.
   ctest -R h2o      # this will run all tests with h2o in their name.
   ctest -L long -j4 # this will run all long tests with 4 jobs at a time.
```

## 8. Problems, bugs, contributions

Let me know at
```
   dylan.jayatilaka@gmail.com
```
I apologise, I am not good at responding. Best to contact some people that know me. If you google you might find such people. There aren't many, as I'm a misanthrope. 

# How to run tonto

See [the wiki](https://github.com/dylan-jayatilaka/tonto/wiki/How-to-run-tonto) for details.
