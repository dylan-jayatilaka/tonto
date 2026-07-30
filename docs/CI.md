# Continuous integration — what runs, and how to run it yourself

Three workflows, in `.github/workflows/`. All of them are free: `tonto` is a public
repository, so GitHub-hosted standard runners have unlimited minutes. What differs
between them is **wall-clock time**, which is why they are not all wired to every push.

| Workflow | File | Badge | Runs | Time |
|----------|------|-------|------|------|
| **CI (Linux)** | `ci.yml` | yes | every push / PR to `antlr4`, `master`, `release` | ~15–20 min |
| **CI (WSL)** | `ci-wsl.yml` | yes | `guards` job on every push; the full WSL build weekly, on demand, and when the WSL machinery changes | ~1 min / ~40–70 min |
| **CI (debug)** | `ci-debug.yml` | **disabled** | manual only | ~15 min |

All three gate on the same **loose** criterion as `scripts/test.py` — relative
error ≤ 0.2 % **or** last printed digit within ±2 — so their verdicts are directly
comparable with each other and with a local `make report`.

---

## Running a workflow by hand

### The prerequisite that catches everyone

**A workflow can only be dispatched manually if a copy of its file containing
`workflow_dispatch` exists on the default branch (`master`).** GitHub looks the
workflow up there, whatever branch you then ask it to run against. If the file only
exists on a feature branch, the *Run workflow* button never appears and the CLI
reports that the workflow has no `workflow_dispatch` trigger.

So while `ci-wsl.yml` and the disabled `ci-debug.yml` live only on `antlr4`, neither
can be started by hand. Merge to `master` first, or rely on the automatic triggers —
pushing to `antlr4` runs `ci-wsl.yml` anyway.

### From the command line

```bash
gh workflow run ci-wsl.yml --ref antlr4 -f run_full_build=true
gh workflow run ci-debug.yml --ref antlr4        # takes no inputs
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

## CI (Linux) — `ci.yml`

The reference build. Ubuntu, `gfortran-14`, release, then the short suite through
`scripts/suite_report.py`. It also runs the **invariant checks**, which compare the
program against itself rather than against a stored reference (spherical vs cartesian
bases must agree below d functions). Those cannot be silently blessed by regenerating
references on a broken build — they exist because a gfortran miscompilation on arm64
macOS went unnoticed for want of exactly such a check.

## CI (WSL) — `ci-wsl.yml`

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
  Runs weekly (Mondays 04:17 UTC), on demand, and on any push touching
  `cmake/WSL.cmake`, `CMakeLists.txt`, `CMakePresets.json`, `scripts/wsl_*.sh` or the
  workflow itself.

The split is about feedback latency and infrastructure flakiness, not money: an hour of
pending checks on every PR, plus the failure modes of installing a distro on a Windows
runner, is noise that isn't about your code.

> **Scheduled runs expire.** GitHub disables `schedule:` triggers in public repositories
> after **60 days without repository activity**. If `antlr4` goes quiet over a break, the
> weekly WSL run stops until someone re-enables it in the Actions tab.

If a runner image ever loses nested virtualisation, WSL2 will fail to start; drop
`wsl-version` to `'1'` in the workflow (WSLv1 has been available since `windows-2019`)
and expect the WSL1 warning to appear in the configure log.

## CI (debug) — `ci-debug.yml`, currently disabled

Builds `-DCMAKE_BUILD_TYPE=debug` and runs two fast jobs to prove the binary executes.
It exists because the debug build is where `USE_PRECONDITIONS`, `-fcheck=bounds`,
`WARN`/`WARN_IF` and non-`PURE` probing live — it is the build used for all debugging,
and it silently rotted once already.

**It was not working, so its automatic triggers are commented out and its badge is
commented out in `README.md`.** A permanently red badge is worse than no badge. Only
`workflow_dispatch` remains, so it can still be run by hand while being fixed.

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

The workflow is still disabled: the fix is verified for the failing step, but the
debug job has not yet been run end-to-end since.

Its scope was always deliberately narrow: it does **not** run the full short suite,
because the debug (`-O0`) build has four longstanding FP-boundary/structural failures
documented in [`../ANTLR4_DEFERRED.md`](../ANTLR4_DEFERRED.md). Widen the scope once
those are triaged.

**To re-enable:** uncomment the `push`/`pull_request` triggers in `ci-debug.yml` and the
badge in `README.md` — both together, or the badge points at a workflow that never runs.
