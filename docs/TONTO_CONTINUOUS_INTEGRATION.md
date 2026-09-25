# Continuous integration

The workflows live in `.github/workflows/`. The repository is public, so GitHub-hosted
runners are free; what differs is wall-clock time, which is why only the quick ones run on
every push.

## What runs

| Workflow | File | Badge | Runs |
|---|---|---|---|
| Linux-release | `ci.yml` | yes | every push / PR to `master` and `develop` |
| Linux-debug | `ci-debug.yml` | yes | every push / PR to `master` and `develop` |
| Linux-MPI | `ci-mpi.yml` | yes | weekly (Mon), on demand, and on pushes touching MPI code |
| WSL-release | `ci-wsl.yml` | yes | `guards` job on every push; the full WSL build weekly (Mon), on demand, and on pushes touching the WSL machinery |
| WSL-debug | `ci-wsl-debug.yml` | yes | weekly (Tue), on demand |
| WSL-MPI | `ci-wsl-mpi.yml` | no | weekly (Fri), on demand |
| macOS-release | `ci-macos.yml` | yes | weekly (Tue), on demand; gfortran-14 and 16 |
| macOS-debug | `ci-macos-debug.yml` | yes | weekly (Thu), on demand |
| macOS-MPI | `ci-macos-mpi.yml` | yes | weekly (Wed), on demand; gfortran-16 with Homebrew Open MPI |
| Full suite | `ci-full-suite.yml`, `ci-full-suite-macos.yml` | no | monthly (1st, 2nd), on demand |
| RGBI tools | `ci-rgbi.yml`, `ci-rgbi-macos.yml` | no | weekly, and on pushes touching `rgbi-scripts/` |
| Release | `release.yml` | — | a `v*` tag: builds and publishes the Linux and Windows binaries |

Badges track `master`. No platform has a parallel *debug* build.

## What each one checks

- **Release** workflows build `-DCMAKE_BUILD_TYPE=reference` — the profile references are
  blessed with, so a red badge means a regression, not a flag difference — and run the
  `short` suite (and `hart`, except under WSL) through `scripts/suite_report.py`, gated on the **loose**
  criterion (relative error ≤ 0.2 % or last printed digit within ±2). They also run the
  invariant checks, which compare the program against itself (spherical against cartesian
  bases, `hart --help` against the options it accepts, Lebedev grids against their order).
  `ci-macos.yml` also asserts the arm64 `-O2` pin on `shell1quartet.F90` is in the flags.
- **Debug** workflows build `debug` and run two quick jobs to prove the binary runs. They do
  not run the suite: the `-O0` build has known floating-point boundary failures, listed in
  `TASKS_AND_HISTORY.md`.
- **MPI** workflows gate on the π rank-invariance check (`scripts/check_mpi_pi.sh`). The suite
  step is informational — the MPI build has open defects (`docs/TONTO_AND_MPI.md`) — so read
  the gate line, not the suite total.
- **Full suite** runs `short`, `long` and `hart`; nothing else runs `long`.

**A skip is an error.** Suite runs pass `--skips-are-errors`, so a test that declines to run
reddens the run. CI runs `suite_report.py`, not `ctest`: a check registered only in
`tests/CMakeLists.txt` is not in CI.

## Running a workflow by hand

A workflow can be dispatched, and its `schedule:` fires, **only if its file is on `master`**.
A workflow that exists only on another branch never runs.

```bash
gh workflow run ci-macos.yml --ref develop
gh workflow run ci-wsl.yml --ref develop -f run_full_build=false   # guards only
gh run list --workflow=ci-macos.yml
gh run view --log-failed
gh run download <run-id>          # tests.log and *.bad outputs
```

Or: **Actions** → the workflow → **Run workflow** → pick a branch.

Every run writes its agreement table to the run's **Summary** page; `tests.log` and any
`*.bad` files are attached as artifacts.

## Changing a workflow

- **The compiler** for Linux-MPI is `FC_VERSION` in `ci-mpi.yml`; the Open MPI cache key
  carries it. The MPI must be built by the same compiler as Tonto, which is why Linux builds
  Open MPI from source and macOS can use Homebrew's.
- **Scheduled runs stop** after 60 days without repository activity; re-enable them in the
  Actions tab.
- **Disabling a badged workflow:** comment out its `push`/`pull_request` triggers (keep
  `workflow_dispatch`) and comment out its badge in `README.md` — both, or the badge points
  at a workflow that never runs.
- **Inside WSL** (`ci-wsl*.yml`): set `core.autocrlf false` before `actions/checkout`, or the
  CRLF guard in `cmake/WSL.cmake` refuses to configure; install `gcc`, which a bare WSL Ubuntu
  lacks; and write `$GITHUB_STEP_SUMMARY`, `$GITHUB_OUTPUT` and `$GITHUB_ENV` from a
  Windows-side step, since they are not visible inside the distro. If a runner loses nested
  virtualisation, set `wsl-version: '1'`.
