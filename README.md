# Welcome to Tonto!

[![CI](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml/badge.svg)](https://github.com/dylan-jayatilaka/tonto/actions/workflows/ci.yml)

# Getting started and Compiling

## 0. Operating systems

These instructions are for Linux which I assume you are using.

### On Linux

For simlicity I'm going to assume you are using Ubuntu, the most popular system (I use it).

It is best to use the the latest version.

I'm also going to assume you are using the command line in a bash terminal, which is the default.

There are instructions for Mac and Windows but I do not know how up-to-date these are as I hardly use them. Follow the links below; otherwise keep reading.

It is possible to cross compile executables on Linux for those other architectures. Though it is easy enough to use `brew`on Mac and WSL on Windows.

### On MacOS

See [Building on MacOS](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-MacOS)

### On Windows

See [Building on Windows](https://github.com/dylan-jayatilaka/tonto/wiki/Building-on-Windows)

## 1. Install `git` and clone the repo

First install `git` 

```
   sudo apt install git
```

You will need to be on the super user list to install software on your machine.

Open a terminal and clone the repository:

```
   git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
```

To compile a particular branch, like the release branch, which is highly recommended (see below) then clone into a named folder:

```
   git clone --recursive https://github.com/dylan-jayatilaka/tonto.git tonto-release
```

## 2. Install other software

While waiting, make sure you have all the other software installed:

The following is recommended: `make`, a Java JDK (for the ANTLR4-based `foo`→Fortran translator; provides `java` and `javac`), `gfortran`, `blas`, `lapack`, `python3` (for testing) and `gnuplot` (for graphs and plots).

```
   sudo apt install make default-jdk gfortran libblas-dev liblapack-dev python3 gnuplot
```

The build fetches the ANTLR4 tool jar automatically (into `external/`) the first
time you run `cmake`, so an internet connection is needed for that first configure.

It is recommended to compile a parallel version of the program via openmpi and friends:

```
   sudo apt install openmpi-bin openmpi-common openssh-client openssh-server libopenmpi-dev 
```

## 3. Switch to the `release` branch, and set up a build folder

To compile Tonto, first enter the `tonto` or `tonto-release` folder you just downloaded

```
    cd tonto
```

Checkout the release branch:

```
   git checkout release
```

Now make a `build` directory (the name is up to you) and enter that to compile the program:

```
   mkdir build
   cd build
```

It is recommended to compile at least two other versions, a `debug` and a `mpi` parallel version each in their own build folders. See below.

## 4. Choose a compiler, and compile executables

You may use different compilers since, unfortunately, some of them are buggy. `gfortran`is pretty good and recommended.

Set your compiler and start compiling:

```
   cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=fast
   make -j
```
Then wait, and when the screen finishes churning, you are done.

**A note on `-j`.** A bare `make -j` launches an unbounded number of jobs. The
Foo→Fortran translation step runs one Java (JVM) process per `.foo` file, and
too many at once can exhaust memory and thrash the machine. It is safer to cap
both the job count and the load average, for example:

```
   make -j4 -l8      # at most 4 parallel jobs, and don't start new ones while load > 8
```

Tune the numbers to your machine (a common choice is `-j` = number of cores and
`-l` = twice that). If a build stalls or the machine becomes unresponsive, lower
`-j` first.

I recommend to also make a `debug` version which prints error messages. In the `tonto` folder type:

```
    mkdir debug
    cd debug
    cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=debug
    make -j

```

If you want a static executable, for redistribution, set the build type to `RELEASE-STATIC` as follows:

```
   mkdir static
   cd static
   cmake .. -DCMAKE_BUILD_TYPE=release-static
   make -j
```

By default the `tonto` program is built with the `RELEASE` flags i.e. not static. The static verssion is a lot larger in size.
  
For production it is recommended to make an MPI parallel version. However you must test this to check that the results are correct. To proceed type:

```
   mkdir mpi
   cd mpi
   cmake .. -DCMAKE_Fortran_COMPILER=mpifort -DCMAKE_CXX_COMPILER=mpicxx -DCMAKE_C_COMPILER=mpicc -DCMAKE_BUILD_TYPE=fast -DMPI=1
   make -j
```

Consider also using `-DNO_ERROR_MANAGEMENT` in this case for even more speed.

## 5. Where is the program?

The executable program is located in the build folder:

```
    build/tonto
```

The standalone Hirshfeld atom refinement terminal (`hart`) program will be located at:

```
   build/hart
```

Copy the program `build/hart` anywhere you like  For help type `hart -help`.

There are other test programs which are made and used.

## 6. Where is the code?

All of these programs are in the `runfiles/` folder. The source code for the modules is in the `foofiles/` folder. The source is currently translated into modern Fortran 2003 which resides in the `build/` folder, which is where the object code and executables are made. The names of the `.foo` modules in the `foofiles/`, and that code, correspond rather directly to the corresponding `.F90` files in the `build/' folder.

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
   vimdiff stdout stdout.bad
```

Here is a nice thing for problem tests: you may use `ctest` in the `build/` folder and run only tests matching certain labels or regular expressions for the jobs that failed; or specify the number of processors to use when running tests :

```
   ctest -L short    # this will run all tests with the label short.
   ctest -R h2o      # this will run all tests with h2o in their name.
   ctest -L long -j4 # this will run all long tests with 4 jobs at a time.
```

### How close is "close enough"? (exact / loose / last-digit)

Because two correct builds can differ in the last printed digits (compiler,
BLAS, grid ordering), the comparison in `scripts/test.py` reports each numeric
difference under **three** criteria instead of a bare pass/fail:

- **exact** — every printed digit is identical.
- **loose** — within a *relative* tolerance (default **0.2 %**) **OR** within a
  *last-digit* tolerance (see below). **This criterion decides pass/fail** (the
  test's exit status), so a test that differs only in known, tiny numerical
  noise still passes.
- **last-digit** — within **±K units** of the last printed decimal place
  (default **K = 2**), which is the sensible band for numbers quoted to low
  precision (e.g. `0.4203` vs `0.4202`).

The tolerances are options on `scripts/test.py` (defaults shown):

```
   --rel-tol         0.002    # loose RELATIVE tolerance (fraction; 0.002 = 0.2%)
   --last-digit-tol  2        # loose LAST-DIGIT tolerance (units of the last place)
   --abs-tol         1e-7     # absolute near-zero floor
```

For example, to insist on a tighter 0.05 % relative match:

```
   python3 scripts/test.py -t tests/short/h2o_rhf_STO-3G -p build/tonto \
           --basis-sets basis_sets --rel-tol 5e-4
```

### A per-suite agreement report (`make report` → `tests.log`)

`ctest` prints a flat pass/fail list and hides each test's stdout (so the concise
`AGREEMENT` line `test.py` prints is not shown; `ctest --output-on-failure` would
dump each failing test's *entire* run, which is far too much). For a compact,
grouped view — a header per suite (`short`, `rgbi`, `long`, `cx`) and the exact /
loose / last-digit verdict plus the worst relative and last-digit deviation in the
last columns — use the reporting driver, wired in as a build target:

```
   cmake --build . --target report      # or, in the build dir:  make report
```

This builds `tonto` if needed, runs `scripts/compare_test_outputs.py` over every
suite, and writes the grouped table to **`tests.log`** in the build directory
(one line per test — not the whole run). You can also run the driver directly:

```
   python3 scripts/compare_test_outputs.py --program build/tonto
```

By default it writes `tests.log` in the current directory as well as printing to
the screen (use `--no-log` for stdout only, or `--log PATH` to redirect). It
accepts the same `--rel-tol`, `--last-digit-tol` and `--abs-tol` options as
`test.py`, plus `--suites` to select a subset:

```
   python3 scripts/compare_test_outputs.py -p build/tonto --suites short rgbi
```

Sample output (also written to `tests.log`):

```
   SUITE: short   (51 tests)
   test                                    exact  loose  lastdig   max rel%    max ulp
   h2o_rhf_STO-3G                          PASS   PASS   PASS             0          0
   h2o_rhf_6-31G(d)_normal_mode_analysis   FAIL   PASS   FAIL        0.0017          3
   ...
   short subtotal:  loose 49/51   (exact 47, lastdig 46)
```

## 7b. Translator analysis tools: call graphs & dead-code elimination

The ANTLR4 `foo`→Fortran translator can analyse the whole source set, not just
translate it. Two developer features build on its cross-module call graph.

### Call & module-use graphs (`make callgraphs`)

From a build directory:
```
   make callgraphs
```
writes into `build/callgraphs/`:

| File | Contents |
|------|----------|
| `call_graph.dot`      | procedure-level call graph, clustered by module (large) |
| `module_use.dot`      | module `use` graph, submodules collapsed into their parent module node |
| `submodule_use.dot`   | expanded submodule `use` graph, one cluster per split family (MOLECULE, DIFFRACTION_DATA) |
| `dead_code_report.tsv`| per-module live/dead procedure counts + the dead list, rooted at `run_molecule` |

Render with Graphviz (install it separately). The two use-graphs are auto-rendered to
SVG if `dot` is on the PATH; the big call graph is best laid out with `sfdp`:
```
   sfdp -Goverlap=prism -Tsvg build/callgraphs/call_graph.dot -o call_graph.svg
   dot  -Tsvg build/callgraphs/module_use.dot -o module_use.svg
```

The graphs are useful documentation in their own right. (First run reconfigures the
build dir once — if `make callgraphs` reports "no rule to make target", run `cmake ..`
in the build dir first, then retry.)

The raw `module_use.dot` is a hairball (139 nodes / 921 edges). To make it readable,
`scripts/simplify_callgraph.py` post-processes it — aggregating array/shell/gaussian
families into single coloured nodes and hiding the handful of universal utility modules:
```
   python3 scripts/simplify_callgraph.py build/callgraphs/module_use.dot --simplify -o simple.dot
   python3 scripts/simplify_callgraph.py build/callgraphs/module_use.dot --module ATOM   # one module's deps
```
`--simplify` drops it to ~50 nodes / 114 edges; `--module NAME` emits a documentation
graph of a single module's direct dependencies (`--reverse` for dependents, `--both` for
both). See [docs/CALL_GRAPHS.md](docs/CALL_GRAPHS.md) for the full rationale (aggregate vs
ambient, the `concentrate` caveat, layout notes).

### Dead-code-eliminated executables (`-DPURGE_DEAD_CODE`)

A given executable only reaches a fraction of the code base; the rest is dead *for that
executable*. Configure a **separate** build tree that emits only the reachable procedures:
```
   mkdir build-slim && cd build-slim
   cmake .. -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release \
            -DPURGE_DEAD_CODE=run_molecule
   make run_molecule
```
This computes reachability from the `run_molecule` (=`tonto`) entry point and drops every
procedure not reachable from it (~1/3 of the ~7600 procedures), producing a smaller binary
that passes the identical test suite. The purge is **per executable** — code dead for
`run_molecule` may be live for `run_dc`/`run_sf`/etc. — so always use a dedicated build tree
and never share it with the normal build. The normal build (no `-DPURGE_DEAD_CODE`) is
unaffected.

Under the hood these use `FooToFortran` flags `--call-graph-report`,
`--dead-code-report <root.foo>`, and `--purge-dead-code <root.foo>` (see `CLAUDE.md §8`).

## 8. Problems, bugs, contributions

Let me know at
```
   dylan.jayatilaka@gmail.com
```
I apologise, I am not good at responding. Best to contact some people that know me. If you google you might find such people. There aren't many, as I'm a misanthrope. 

# How to run tonto

See [the wiki](https://github.com/dylan-jayatilaka/tonto/wiki/How-to-run-tonto) for details.

# Developers: pushing to GitHub with a token

To push to GitHub over HTTPS you authenticate with a **Personal Access Token**
(PAT) — GitHub no longer accepts your account password on the command line.

### 1. Create a token

Go to <https://github.com/settings/tokens> (or navigate: profile photo →
**Settings** → **Developer settings** → **Personal access tokens** → **Tokens
(classic)** → **Generate new token (classic)**). Give it the **`repo`** scope,
set an expiry, and generate it. **Copy the token immediately** — GitHub shows it
only once.

### 2. Point your local repo at it

Set the `origin` URL to embed your username and token:

```
git remote set-url origin https://USERNAME:TOKEN@github.com/USERNAME/REPO.git
```

Replace `USERNAME` with your GitHub username, `TOKEN` with the token you just
copied, and `REPO` with the repository (e.g. `tonto`). Pushes will now
authenticate automatically.

### 3. Keep the token safe

The token is a password — anyone who has it can push as you. Embedding it in the
remote URL stores it in **plaintext** in `.git/config`, and it is visible to
anyone who runs `git remote -v`, so treat that clone as sensitive and never share
its URL or paste it into logs/issues. If a token ever leaks, revoke it at the
link above and issue a new one.

**More secure alternative — SSH keys.** SSH keeps no secret in the URL: you
generate a key pair once, add the *public* key to GitHub, and set `origin` to
`git@github.com:USERNAME/REPO.git`. See GitHub's
[Connecting to GitHub with SSH](https://docs.github.com/en/authentication/connecting-to-github-with-ssh)
guide.

# Erice 2025 workshop people!

If you are a workshop attendee, the lab folder and instructions are [here](https://drive.google.com/drive/folders/17OWncmSsFbKAlW8mZb9EKzJuW0GAjykG).
