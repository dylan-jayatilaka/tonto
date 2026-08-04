# Continuous integration — what runs, and how to run it yourself

Four workflows, in `.github/workflows/`, named `(platform)-(build type)`. All of them
are free: `tonto` is a public repository, so GitHub-hosted standard runners have
unlimited minutes. What differs between them is **wall-clock time**, which is why they
are not all wired to every push.

| Workflow | File | Badge | Runs | Time |
|----------|------|-------|------|------|
| **CI (Linux-release)** | `ci.yml` | yes | every push / PR to `antlr4`, `master`, `release` | ~15–20 min |
| **CI (WSL-release)** | `ci-wsl.yml` | yes | `guards` job on every push; the full WSL build when the WSL machinery changes, on demand, and weekly (Mon) *once on `master`* | ~1 min / ~40–70 min |
| **CI (Linux-debug)** | `ci-debug.yml` | yes | every push / PR to `antlr4`, `master`, `release`, and on demand | ~15 min |
| **CI (WSL-debug)** | `ci-wsl-debug.yml` | yes, but **never yet run** | weekly (Tue) and on demand — both need it on `master` first | ~60–90 min |

The two release workflows gate on the **loose** criterion from `scripts/test.py` —
relative error ≤ 0.2 % **or** last printed digit within ±2 — so their verdicts are
directly comparable with each other and with a local `make report`. The two debug
workflows deliberately do **not** run the suite at all; see below.

One wrinkle in the naming: the fast `guards` job lives inside **CI (WSL-release)**
even though what it tests (`cmake/WSL.cmake`) is not release-specific. It is there so
that WSL work gets a signal on every push without a fifth badge.

---

## Running a workflow by hand

### The prerequisite that catches everyone

**A workflow can only be dispatched manually if a copy of its file containing
`workflow_dispatch` exists on the default branch (`master`).** GitHub looks the
workflow up there, whatever branch you then ask it to run against. If the file only
exists on a feature branch, the *Run workflow* button never appears and the CLI
reports that the workflow has no `workflow_dispatch` trigger.

**The same rule governs `schedule:`,** and it is easier to miss: cron triggers fire
**only from the default branch** too. A weekly job defined on a feature branch does not
run weekly — it does not run at all.

Both consequences were live until **2026-08-04**, when `antlr4` was merged into `master`
and `release`. All four workflow files now exist on the default branch, so every one of
them can be started by hand, and the Monday (WSL-release) and Tuesday (WSL-debug) crons
fire for real. Before that merge they existed only on `antlr4`, and the crons were inert
— if you ever add a workflow on a feature branch again, expect the same silence.

### From the command line

```bash
gh workflow run ci-wsl.yml --ref release -f run_full_build=true
gh workflow run ci-wsl-debug.yml --ref release   # takes no inputs
gh workflow run ci-debug.yml --ref release       # takes no inputs
```

`run_full_build` is the one input `ci-wsl.yml` defines: leave it true to include the
long Windows job, set it false to run only the fast `guards` job.

Then watch it:

```bash
gh run list --workflow=ci-wsl.yml     # recent runs, newest first
gh run watch                          # live progress of the latest
gh run view --log-failed              # only the failing step's log
gh run view <run-id> --log            # everything
gh run download <run-id>              # the tests.log / *.bad artifacts
```

### From the web

Repository → **Actions** tab → choose the workflow in the left sidebar → **Run
workflow** (top right of the run list) → pick a branch → **Run workflow**.

### Reading the result

Every workflow writes its agreement table to the run's **Summary** page, so the
per-test Exact / Loose / Last-digit columns are visible without opening a step log
or downloading anything. `tests.log` and any `*.bad` outputs are attached as
artifacts on both success and failure.

---

## CI (Linux-release) — `ci.yml`

The reference build. Ubuntu, `gfortran-14`, release, then the **short** and **hart**
suites through `scripts/suite_report.py`. It also runs the **invariant checks**, which
compare the program against itself rather than against a stored reference (spherical vs
cartesian bases must agree below d functions; `hart --help` must list exactly the options
`run_har.foo` accepts). Those cannot be silently blessed by regenerating references on a
broken build — they exist because a gfortran miscompilation on arm64 macOS went unnoticed
for want of exactly such a check, and because `hart` shipped for years with a documented
option its code rejected.

## CI (WSL-release) — `ci-wsl.yml`

Two jobs, because they cost very different amounts of wall-clock. See
[`BUILD_WSL.md`](BUILD_WSL.md) for what is being guarded and why.

- **`guards`** — ubuntu-latest, ~1 min, **every push**. Every WSL failure condition is
  path- or file-shaped, so `scripts/wsl_selftest.sh` can simulate all of them without a
  Windows machine; it asserts exit status *and* message for 14 cases. The job also
  checks that an ordinary non-WSL configure is completely unaffected. This is what the
  badge tracks day to day.
- **`wsl-build`** — a real WSL2 Ubuntu on a Windows runner, ~40–70 min. Re-checks the
  guards against a genuine drvfs mount and interop `PATH`, copies the checkout off
  `/mnt` into `~`, builds, and runs the short suite with the same gate as Linux CI.
  Runs on any push touching
  `cmake/WSL.cmake`, `CMakeLists.txt`, `CMakePresets.json`, `scripts/wsl_*.sh` or the
  workflow itself; on demand; and weekly (Mondays 04:17 UTC) once the file is on the
  default branch — see the schedule caveat above.

The split is about feedback latency and infrastructure flakiness, not money: an hour of
pending checks on every PR, plus the failure modes of installing a distro on a Windows
runner, is noise that isn't about your code.

> **Scheduled runs expire.** GitHub disables `schedule:` triggers in public repositories
> after **60 days without repository activity**. If the repository goes quiet over a
> break, the weekly WSL run stops until someone re-enables it in the Actions tab.

If a runner image ever loses nested virtualisation, WSL2 will fail to start; drop
`wsl-version` to `'1'` in the workflow (WSLv1 has been available since `windows-2019`)
and expect the WSL1 warning to appear in the configure log.

### What the first real run found: `gcc`

Run 30581657238 brought WSL2 up fine and then failed in the guard-assertion step:

```
::error::rejected, but not for the DrvFs reason
CMake Error at CMakeLists.txt:16 (project):
  No CMAKE_C_COMPILER could be found.
```

Tonto is `project(tonto LANGUAGES Fortran C)`, and a bare WSL Ubuntu image has no C
compiler — `gfortran-14` pulls in `gcc-14-base` but not the `gcc` driver. Ubuntu CI
runners ship one preinstalled, so only the WSL jobs could ever hit this. Fixed by
adding `gcc` to `additional-packages`, to the apt line in `BUILD_WSL.md`, and as a
check in `scripts/wsl_doctor.sh`.

Worth noting *how* it was caught: the assertion checks the error **message**, not just
the exit status. `cmake` did fail, and a status-only check would have called that a
pass and moved on to a build that could never work. `project()` runs before
`include(WSL)`, so a broken toolchain always reports before the WSL guards get a turn.

### …and what the second one found: CRLF

With `gcc` installed, run 30582532289 got through WSL setup, the guard assertions
against a genuine drvfs mount and interop `PATH`, and the copy into `~` — then failed
in `Configure`, with the CRLF guard firing on `foofiles/types.foo`.

The guard was **correct**. `actions/checkout` runs *Windows* git on a Windows runner,
and its default `core.autocrlf=true` rewrites every file to CRLF on checkout. Copying
that into WSL reproduces trap 3 from [`BUILD_WSL.md`](BUILD_WSL.md) exactly, and
`cmake/WSL.cmake` refused to configure — which is precisely its job.

Fixed by configuring git **before** the checkout step, in both WSL workflows:

```yaml
- run: |
    git config --global core.autocrlf false
    git config --global core.eol lf
- uses: actions/checkout@v4
```

### …and the third: `$GITHUB_STEP_SUMMARY` does not exist inside WSL

Run 30582988092 configured **completely successfully** — CMake found LAPACK 3.12.0, every
WSL guard passed, `Build files have been written` — and then the step failed anyway on:

```
GITHUB_STEP_SUMMARY: unbound variable
```

`$GITHUB_STEP_SUMMARY` is a **Windows-side** variable and is not exported into the WSL
distro, and the `wsl-bash` wrapper runs `bash --noprofile --norc -euo pipefail`, so `-u`
turns merely referencing it into a fatal error. Nothing to do with Tonto.

Fixed by having the WSL steps stage their summary into `wsl-summary.md` in the workspace,
and adding a Windows-side step (default `pwsh`, where the variable *is* in scope) that
appends it. **Anything written from inside `wsl-bash` must follow this pattern** — the
same applies to `$GITHUB_OUTPUT` and `$GITHUB_ENV`.

That run also confirmed the rest of the machinery on real hardware: WSL 2 comes up on
`windows-latest` (kernel `6.18.33.2-microsoft-standard-WSL2`, so nested virtualisation
is available and the `wsl-version: '1'` fallback is not needed), the PATH sanitiser
dropped **72** Windows directories, a Linux `/usr/bin/cc` and JDK were selected, and
the memory advisory computed `make -j3` from 4 CPUs / 7 GB.

## CI (WSL-debug) — `ci-wsl-debug.yml`

The debug counterpart of CI (WSL-release), and the WSL counterpart of CI (Linux-debug):
a `-DCMAKE_BUILD_TYPE=debug` build inside a real WSL2 Ubuntu, followed by the same two
fast smoke jobs as `ci-debug.yml`. Weekly on Tuesdays (a day after WSL-release, so two
hour-long Windows jobs never queue against each other) and on demand — both of which
require the file on `master` first. **No push trigger** — nothing gates on it.

There is little point dispatching this before CI (WSL-release) is green. The two share
nearly everything that can break — the LF checkout, `setup-wsl` and `gcc`, the copy into
`~`, the summary staging, and the 184-file translation — and differ only in the `-O0`
flags and in running two smoke tests instead of the suite. Until the shared path works,
this job would just rediscover the same failures on a second 7 GB Windows runner.

Under WSL there is more to rot than on Linux: a debug build is far more I/O- and
fork-heavy than a release one, which is what WSL is worst at.

> **This workflow has never had a green run.** It was written alongside the WSL-release
> job and shares its setup, but the debug path through WSL is unproven. Treat a first
> red run as "needs triage", not "the debug build is broken".

## CI (Linux-debug) — `ci-debug.yml`

Builds `-DCMAKE_BUILD_TYPE=debug` and runs two fast jobs to prove the binary executes.
It exists because the debug build is where `USE_PRECONDITIONS`, `-fcheck=bounds`,
`WARN`/`WARN_IF` and non-`PURE` probing live — it is the build used for all debugging,
and it silently rotted once already.

It was switched off on 2026-07-30 because it was failing, and **re-enabled the same day**
once the cause was found and fixed (below). Its badge is live again.

### The failure, and its fix

Its one and only run (30571546632) built fine and then died in the smoke test with

```
FileNotFoundError: [Errno 2] No such file or directory: 'build-debug/tonto'
```

while the diagnostics step, three seconds later in the same directory, happily
`ls`-ed a 100 MB `build-debug/tonto`. The binary was never missing: `run_test()` in
`scripts/test.py` chdir's into a temp directory before launching the program, so a
**relative** `--program` is resolved from there and vanishes. `main()` already
absolutised `--sbftool`, `--test-directory` and `--basis-sets` for exactly this
reason — `--program` had been left out of that list, and the default `./tonto` had
the same flaw. `ci.yml` was unaffected only because it happens to pass
`-p "$PWD/build/tonto"`.

Fixed in `scripts/test.py` by absolutising `--program` alongside the others (a bare
name with no separator is left alone, so it can still be found on `PATH`). Verified
locally both ways: `--program release/tonto` now passes where it previously raised
`FileNotFoundError`, and an absolute path — what `ctest` passes — still works.

The workflow was re-enabled once that fix was in. The fix is verified for the step that
failed; the first full green run is still pending, so if the badge is red, read the run
before assuming the debug build itself is broken.

Its scope was always deliberately narrow: it does **not** run the full short suite,
because the debug (`-O0`) build has four longstanding FP-boundary/structural failures
documented in [`../DEFERRED.md`](../DEFERRED.md). Widen the scope once
those are triaged.

**To disable again**, if it turns red and you want the noise gone: comment out the
`push`/`pull_request` triggers in `ci-debug.yml` (leave `workflow_dispatch`) and wrap the badge
in `README.md` in an HTML comment — both together, or the badge points at a workflow that never
runs.
