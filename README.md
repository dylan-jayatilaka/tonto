# Welcome to Tonto!

## 1. Get ready ...

* If you don’t want to develop just download the zip file using the button on
  the right.
* If you want to develop first install `git` and then clone the repository
  (which may take some time)

### On Linux
```
   git clone https://github.com/dylan-jayatilaka/tonto.git
```

While waiting you can install:

* `perl`
* `gfortran`
* `make`

If you intend making a parallel version install:

* `mpich2`

If you intend making plots we recommend to install:

* `gnuplot`

Your text editor is a personal choice. If you use `vim`
you might want to install

* `exuberant-tags`
* `cscope`

With these and `vim` you can syntax highlight, jump to
tags under your cursor (e.g. routines, types, or macros) and
have Tonto-library-specific code completion.

### On Windows

The compilation procedure is the same as above, but you have to first
install the [cygwin](https://cygwin.com/install.html) unix
environment.

The programs tend to be slower when running on Windows.

You will need to install the following packages from
the menu:

* `perl`
* `gcc-core`
* `gcc-fortran`
* `make`

And perhaps (see above) also:

* `vim`
* `ctags`
* `cscope`
* `gnuplot`

### On Mac

The compilation procedure is the same as above, but you have to first
install the [brew](http://brew.sh/) package manager.
environment.

Then install:

* `perl`
* `gcc`
* `make`

## 2. Get set ...

To compile Tonto, type :

```
    mkdir build && cd build
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

By default the `tonto` program on Linux is the executable program

```
    build/run_molecule
```

On other platforms it will be different, but of the form

```
    build/run_molecule(.exe)
```

Here `build` is usually `fast` but may be `debug` depending on if
you change the compilation options in the platform-specific
file -- see below.

If you want to build the standalone Hirshfeld atom refinement terminal
(HARt) program then type:

```
   make run_har.exe
```

Copy the program `run_har.exe` anywhere you like and call it `HARt` !
For help type `HARt -help`.

## 3. Go!

To run the tests, type:

```
   make tests
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

2. Type  `./<your-compiler-on-your-OS>/<build>/run_molecule.exe`

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

# Custom executables

Still here? :-)

It is possible to compile specific versions of Tonto executables ---
fast versions, debug version or custom compiles.

## Compactification

Executables in Tonto tend to be big. If your executable is too big,
you can remove all "dead code" from your application by typing:

```
   make compactify
   make
```

Consider also using the -DNO_GENERIC_NAMES compile option. See below.

## Custom compile options

By default, a fast production version of Tonto is compiled.

You can also compile “debug” and “custom” versions of of Tonto using
the compile switches described in the platform-specific compiler
options file. This file is located in the `platforms/` directory, and it
has a name like `<your-compiler-on-your-OS>` e.g. on Linux

```
   platforms/GNU-gfortran-on-LINUX.
```


You change the `$(FOPTNS)` in this file to get different results e.g.
changing it to

*  `$(FFAST)`
   ... means the executables will be placed in the `fast/` subdirectory of
   the platform-specific directory.

*  `$(FDEBUG)`
   ... means the executables will be placed in the `debug/` subdirectory of
   the platform-specific directory. Useful for getting error messages
   which I may ask you for if there is trouble.

*  `$(FPROF)`
   ... means the executables will be placed in the `custom/` subdirectory of
   the platform-specific directory. You can also change the `$(FPROF)`
   to include your desired fortran compiler options. This is useful for
   profiling slow parts of code, and for developing.

## Parallelism and more custom options

There are also other kinds of compile options which control the type
of Fortran code which is generated. To actiavte these options you
just add them to the $(DEFS) variable in the compiler options file.

A summary of switches which control the type of Fortran generated is
given below. You can use multiple options if it is sensible to do so.

Switch                      | Meaning
----------------------------|-----------------------------------------
-DMPI                       | Parallel Tonto, using MPI. If using this
                            | option, you must specify your specific
                            | MPI libraries in the platforms fil by
                            | setting the $(LIBS) variable e.g. -lmpi
                            |
-DFLUSH                     | Flush output to stdout. The compiler must
                            | have the flush() command.
                            |
-DNO_TONTO_SYSTEM_CALLS     | No system checks, no parallel calls
                            |
-DNO_TONTO_SYSTEM_CHECKS    | No system checks, retain parallel calls.
                            | You must still define MPI
                            |
-DNO_GENERIC_NAMES          | Prepend module name to routines. This may
                            | reduce compile time by reducing the
                            | namespace, but will have nonstandard
                            | routine names > 31 characters long.
                            |
-DUSE_ERROR_MANAGEMENT      | Use DIE and WARN macros i.e. the minimal
                            | amount of error checking (default)
                            |
-DUSE_PRECONDITIONS         | Use routine preconditions.This is more
                            | robust error checking, but there will be
                            | a perfomance hit. This option implies the
                            | use of USE_ERROR_MANAGEMENT.
                            |
-DUSE_CALL_STACK_MANAGEMENT | Tracks the call tree.  Good for tracking
                            | errors and warnings. Big performance hit.
                            | If possible, try to use the compilers own
                            | instruments. This option imples the use
                            | of USE_PRECONDITIONS
                            |
-DTRACK_MEMORY_LOCATIONS    | Tracks all memory allocation and
                            | deallocation. Useful for eliminating
                            | memory leaks.  Compiler must have loc()
                            | pointer function. Consider using the
                            | compilers own options e.g. the g95
                            | compiler. This option implies the use of
                            | USE_CALL_STACK_MANAGEMENT
                            |
-DUSE_TIME_PROFILING        | Time profile routines. Not very accurate.
                            | Consider using the compiler profiling
                            | tools. 
                            |
-DNO_CASE_OPTIONS           | Removes informative error on string-based
                            | case statements i.e. removes listing of
                            | allowed keywords in Tonto.
