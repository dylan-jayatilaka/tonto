# Welcome to Tonto!

Tonto now uses `cmake` for all builds on github.

## 1. Get ready ...

* If you don’t want to develop just download the zip file using the button on
  the right.
* If you want to develop first install `git` and then clone the repository
  (which may take some time)

### On Linux
```
   git clone https://github.com/dylan-jayatilaka/tonto.git
```

While waiting, in another terminal window, you can install:

* `perl`
* `gfortran`
* `make`
* `numdiff`
* `blas` 
* `lapack` 
* `mpich2` (for parallel)
* `gnuplot` (recommended)

Your text editor is a personal choice. If you use `vim`
you might want to install

* `exuberant-tags`
* `cscope`
* `gnuplot`

With these and `vim` you can syntax highlight, jump to
tags under your cursor (e.g. routines, types, or macros) and
have Tonto-library-specific code completion.

### On Mac

The compilation procedure is the same as for Linux, but you have to first
install the [brew](http://brew.sh/) package manager, and then use that
to install `git` and all the other GNU Linux software.

### On Windows

The compilation procedure is similar to Linux, but you have to first
install the [MSYS2](https://msys2.github.io) minimal unix environment.

IMPORTANT: if you have a [cygwin](https://cygwin.com/install.html)
unix environment, please delete it before installing MSYS2. You will
find cygwin under in `C:/cygwin` or something like that. We have found
that cygwin interferes with MSYS2.

Once you have MSYS2, open it's console; it has an "M" logo and looks a
bit purple. Do not confuse it with the MS console. You can find it
in `C:/mingw64`.

In the MSYS2 console, update the database 

```
pacman -Syu
```

To be safe, close the MSYS2 console, open it again and type

```
pacman -Su
```

If you are on an `x86_64` machine (most likely) install:

```
pacman -S mingw-w64-x86_64-toolchain
```

Else if you are on an `ì686` machine then install:

```
pacman -S mingw-w64-i686-toolchain
```

Now install this stuff:

```
pacman -S base-devel
pacman -S perl
pacman -S git
pacman -S make
pacman -S mingw-w64-x86_64-cmake
```

Note that the `make` installed above is the MSYS2 `make` and
not the GNU `make`.

Now compile and install the `openblas` library which includes `lapack`:

```
git clone "https://github.com/Alexpux/MSYS2-packages"
cd MSYS2-packages/mingw-w64-openblas
makepkg -sLf
pacman -U mingw-w64-openblas-*.pkg.tar.xz
```

The above will take a long time.

Finally, update the `PATH` variable by editing the `.bashrc` file,
e.g. using the `vim` editor, and add the following line at the end:

```
export PATH=$PATH:/ming64/bin
```

And source the file:

```
source ./.bashrc
```


## 2. Get set ...

To compile Tonto, first go into the `tonto` directory downloaded with `git`

```
    cd tonto
```

Then make a `build` directory and go into it:

```
    mkdir build && cd build
```

Of course you can change `build` to be e.g. `gfortran` after the
name of the compiler you use. Executables from different compilers
or with different compilation options can be produced in build 
directories with different names. That's convenient.

Now you can build the programs :

```
    cmake ..
    make -j
```

If you want a specific compiler, type:

```
   cmake .. -DCMAKE_Fortran_COMPILER=<insert-your-compiler-here>
   make -j
```

where you should replace <insert-your-compiler-here> with the
command for your fortran compiler.

To make an MPI parallel version type :

```
   cmake .. -DMPI=1
   make -j
```

To change build type (e.g. make a DEBUG version) type:

```
   cmake .. -DCMAKE_BUILD_TYPE=Debug
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

By default, the tests use the `numdiff` program
to check the difference between outputs.

To run all tests, in the build directory type:
```
   make tests # this will run all tests
```
Or you may use `ctest` directly and run only tests matching
certain labels or regular expressions. You may also
specify the number of processors to use when running tests.
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

## Example input

Look in the directories in the `tests/`` folder for examples.

* Example input files are always called `stdin` 

* Example output files are always called `stdout`. 

* Some of these examples need auxiliary input data files, which are
  also there.

* The file called `IO` lists the auxiliary files needed as input. 
  It also lists temporary files produced while running the program
  that should be deleted.

## Running tonto

1. Make an input file called `stdin` ... or copy one you like from the
   `tests/` folder

2. run the `tonto` executable from the build directory.

Congratulations --- you just ran Tonto!

## Practical set-up

Of couse you can copy this executable wherever you like. 

* Generally you will also need to copy the `basis_sets` folder to the
  same place (or else tell where it is in the `stdin` input file). 

* I usually make a symbolic link (or alias) with the name `tonto`.
  Imaginative. This way, if you recompile after an update all your
  versions are up-to-date. 

* For long jobs, it is worthwhile to give a particular program a
  descriptive name like `tonto.this-is-for-oxalic-acid`. Then if you
  have multiple jobs runnung you can differentiate them.

# Viewing and writing code

Tonto was written for coders. 

It’s clean and elegant and highly documented.

## Foo

Tonto is written in the `Foo` language. It stands for object oriented
`Fortran` in reverse.

`Foo` is identical to `Fortran` at the expression level but differs
in the way variable, subroutines and functions are declared.
`Foo` looks like python with declarations, and it translates into
`Fortran03`. If you want to use it you should know _modern_ and
_object oriented_ `Fortran`. And it would help if you knew Python or
any other object oriented language.

These web pages at the NCI provide a remarkably short and excellent
introduction to `Fortran`

*  http://nci.org.au/services-support/training/fortran-programming-basics/
*  http://nci.org.au/services-support/training/fortran-programming-advanced/

The `Foo` language is described on the Tonto web pages. However, most
people will be able to pick it up by simply browsing the code which is
written to be understood --- and if not, then commented.

## Jumping around in code with `vim`

Tonto is a vast library. 

To browse it effectively use the `vim` editor. That is because syntax
highlighting has been set up for `vim` well as completion of routine
names in the library. Besides, `vim` is a great editor. :-)

To get started with `vim` copy or link the supplied `.vimrc` and
`.vim` directories in the Tonto root directory to your home directory,
as shown below. Backup or merge your own files if needed.

```
   ln -s .vim ~
   ln -s .vimrc ~
```

Then start up `vim` on one of the files in `foofiles/` from the Tonto
root directory, say

```
   vim foofiles/buffer.foo
```

Here are some key points:

* To unfold routines type `zo`. 

* To close or fold a routine type `zc`.

* To jump to different routines while the cursor is placed on top of
  the routine name run the script

```
   scripts/cscope_setup
```

  Then, while editing a file `foofiles/<some-file>.foo` with `vim`
  from the Tonto **root directory** type:

```
   g Ctrl-]
```

   when the cursor is on top of the routine name or type component you
   are interested in. If there are multiple routines with the same name,
   choose the one you really want to see. Otherwise you will jump
   straight to the definition of that routine.

To jump back to where you were, Type "Ctrl-t". Wonderful!

## Code completion

Finally, for code completion. This is cool.

When editing a .foo file with vim, set up .ctags as above and type:

```
   :FooCompleteOn
```

Now try adding a dot after a variable, and while still in insert
mode, after the dot type

```
   Ctrl-X Ctrl-O
```

You will see a pop-up menu with the available object methods. Type a
few more characters to narrow down the choice. The method is listed
as a type-component/attribute (a), subroutine (s), function (f),
type-definition (t), macro (m), or global variable (g).  For more
details type

```
   :help completion
```

and look at the omni completion documentation.

Can somebody help me do the same for the emacs editor?

Or any other trendy one that people are using these days?

