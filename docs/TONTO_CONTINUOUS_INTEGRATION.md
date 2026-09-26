# Continuous integration

GitHub builds and tests Tonto automatically. Each set of checks is a *workflow*, described
by a file in `.github/workflows/`. GitHub runs them free of charge because the repository is
public. Some take over an hour, so only the quick ones run every time a change is pushed; the
rest run weekly or monthly.

## What runs

| Workflow | File | Badge | When it runs |
|---|---|---|---|
| Linux-release | `ci.yml` | yes | every push to `master` or `develop` |
| Linux-debug | `ci-debug.yml` | yes | every push to `master` or `develop` |
| Linux-MPI | `ci-mpi.yml` | yes | Mondays, on request, and when MPI code changes |
| WSL-release | `ci-wsl.yml` | yes | a one-minute check every push; the full Windows build on Mondays, on request, and when the WSL build files change |
| WSL-debug | `ci-wsl-debug.yml` | yes | Tuesdays, on request |
| WSL-MPI | `ci-wsl-mpi.yml` | no | Fridays, on request |
| macOS-release | `ci-macos.yml` | yes | Tuesdays, on request; with gfortran-14 and gfortran-16 |
| macOS-debug | `ci-macos-debug.yml` | yes | Thursdays, on request |
| macOS-MPI | `ci-macos-mpi.yml` | yes | Wednesdays, on request; gfortran-16 with Homebrew's Open MPI |
| Full suite | `ci-full-suite.yml`, `ci-full-suite-macos.yml` | no | on the 1st and 2nd of each month, on request |
| RGBI tools | `ci-rgbi.yml`, `ci-rgbi-macos.yml` | no | weekly, and when `rgbi-scripts/` changes |
| Release | `release.yml` | — | when a version tag `v*` is pushed: builds the Linux and Windows downloads |

The badges on the README show the latest result on `master`. No platform yet tests a
parallel *debug* build.

## What each one checks

- **Release workflows** build Tonto with the same compiler settings used to make the stored
  reference outputs (`-DCMAKE_BUILD_TYPE=reference`). So when a test fails, Tonto's results
  have changed; it is not because the compiler settings differ. They run the `short` tests,
  and the `hart` tests except under WSL. A test passes if every number in its output agrees
  with the reference to within 0.2%, or to within 2 in the last printed digit.
- **Release workflows also run self-consistency checks**, which need no stored reference: a
  spherical and a cartesian basis must give the same answer where they should, `hart --help`
  must list exactly the options `hart` accepts, and each Lebedev grid must integrate exactly to
  its stated order. On a Mac, `ci-macos.yml` also checks that `shell1quartet.F90` is still
  compiled at `-O2`, which Apple silicon needs.
- **Debug workflows** build the debug version and run two quick jobs, to show that it builds
  and runs. They do not run the test suite, because the debug build has some known failures
  (listed in `TASKS_AND_HISTORY.md`).
- **MPI workflows** pass if π comes out the same on 1, 2 and 4 processes
  (`scripts/check_mpi_pi.sh`). They run the test suite too, but only for information: the
  parallel build still has known faults (`docs/TONTO_AND_MPI.md`). Read the π line, not the
  suite total.
- **The full suite** runs the `short`, `long` and `hart` tests. It is the only workflow that
  runs `long`.

A test that does not run at all — for example because `numpy` is missing — counts as a
failure. The workflows run the tests with `scripts/suite_report.py`, not `ctest`, so a check
added only to `tests/CMakeLists.txt` is not tested on GitHub.

## Running a workflow by hand

GitHub can only run a workflow — by hand or on its weekly schedule — **if its file is on the
`master` branch**. A workflow that exists only on another branch never runs.

```bash
gh workflow run ci-macos.yml --ref develop                        # run it on develop
gh workflow run ci-wsl.yml --ref develop -f run_full_build=false  # the one-minute WSL check only
gh run list --workflow=ci-macos.yml                               # recent results
gh run view --log-failed                                          # what went wrong
gh run download <run-id>                                          # tests.log and the *.bad outputs
```

Or on the website: **Actions** → choose the workflow → **Run workflow** → choose a branch.

Each run shows its table of test results on the run's **Summary** page. The full log,
`tests.log`, and the output of any failed test, `*.bad`, can be downloaded from the run.

## Changing a workflow

- **The compiler for Linux-MPI** is set by `FC_VERSION` in `ci-mpi.yml`. The MPI library must be
  built with the same compiler as Tonto, so the Linux workflow builds Open MPI itself and keeps
  it for later runs; changing `FC_VERSION` makes it rebuild. macOS can use Homebrew's Open MPI.
- **Weekly runs stop** if the repository has no activity for 60 days. Turn them back on in the
  Actions tab.
- **To switch off a workflow that has a badge,** comment out its `push` and `pull_request`
  lines (keep `workflow_dispatch`, so it can still be run by hand) and comment out its badge in
  `README.md`. Do both, or the badge keeps showing an old result.
- **The WSL workflows** (`ci-wsl*.yml`) have three traps. Git on Windows must be told not to
  convert line endings (`core.autocrlf false`) before the checkout step, or the WSL build
  refuses to start. A bare WSL Ubuntu has no C compiler, so `gcc` must be installed. And the
  GitHub variables for the run summary (`$GITHUB_STEP_SUMMARY`, `$GITHUB_OUTPUT`, `$GITHUB_ENV`)
  are not visible inside WSL, so they must be written from a Windows step. If WSL 2 fails to
  start on GitHub's Windows machines, set `wsl-version: '1'`.
