# Building Tonto on Windows, via WSL

WSL (Windows Subsystem for Linux) runs a real Ubuntu on Windows, and Tonto builds
there exactly as it does on Linux — **provided you avoid four traps that WSL adds
and plain Ubuntu does not have.** Each of them fails late and with an error message
that points nowhere near the cause, so `cmake` now checks for all four and stops
with the fix in the message (`cmake/WSL.cmake`).

If you just want the commands, skip to [Quick start](#quick-start).

---

## Quick start

From a Windows PowerShell prompt, once:

```powershell
wsl --install -d Ubuntu-24.04
```

Then, **inside** the Ubuntu shell:

```bash
sudo apt update
sudo apt install -y gfortran-14 libblas-dev liblapack-dev default-jdk \
                    python3 perl make cmake git gnuplot

# Clone into your Linux home -- NOT into /mnt/c. See "Where to put the code".
cd ~
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto

# Check the environment before spending an hour on a build:
scripts/wsl_doctor.sh

# Build (the presets put the build tree under ~/tonto-build, off the Windows drive):
cmake --preset wsl-release
cmake --build --preset wsl-release -- -j4      # see "How much parallelism"

# Test:
ctest --preset wsl-short
```

`cmake --preset wsl-release` is equivalent to:

```bash
cmake -B ~/tonto-build/release -S ~/tonto \
      -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release
```

---

## The four traps

### 1. A Windows JDK gets picked up instead of the Linux one

WSL appends the **Windows** `PATH` to the Linux one. If you have a JDK installed on
the Windows side — very common — then `java` and `javac` resolve to
`/mnt/c/Program Files/.../java.exe` unless a Linux JDK shadows them.

That matters here more than in most projects, because the Foo→Fortran translator is
a Java program that CMake invokes with Linux paths and a `:`-separated classpath.
A Windows JDK can read neither: it cannot open `/home/you/tonto/foofiles/types.foo`,
and it wants `;` between classpath entries. What you actually see is an obscure
ANTLR or "class not found" error, several minutes into the build.

**What CMake does:** before searching for any tool, it removes every `/mnt/<drive>/`
directory from `PATH`. That fixes it for `find_package(Java)`, `find_program(dot)`
and everything else at once. It then rejects a JDK under `/mnt/` or ending in `.exe`
as a backstop, in case one was passed explicitly or is left in a stale `CMakeCache.txt`.

**What you do:** `sudo apt install default-jdk`. Nothing else.

If you would rather WSL never put Windows directories on your `PATH`, add this to
`/etc/wsl.conf` and run `wsl --shutdown` from Windows:

```ini
[interop]
appendWindowsPath = false
```

### 2. Building on the Windows drive

Files under `/mnt/c` (or any `/mnt/<letter>`) live on the Windows filesystem, reached
through a translation layer. It is **10–50× slower** than the Linux filesystem, the
executable bit is unreliable, and symlinks are not real symlinks. Tonto translates
184 `.foo` files and then compiles the result — close to the worst possible workload
for it.

**What CMake does:** a build tree on a Windows drive is a hard error. A *source* tree
on one is only a warning: no path in the repository is illegal on NTFS and there are
no case-collisions, so it does work — it is just slow.

**What you do:** keep both in your Linux home (`~`). If your source really must live
on the Windows side so that Windows editors can see it, at least put the build tree
in `~`:

```bash
cmake -B ~/tonto-build/release -S /mnt/c/Users/you/tonto ...
```

You can reach your Linux files from Windows Explorer at `\\wsl$\Ubuntu-24.04\home\you`,
which is usually the better way round.

### 3. CRLF line endings

If the repository is cloned by **Windows** git with `core.autocrlf=true` and then built
from WSL, every `.foo` file carries `\r` at the end of each line. The translator and
gfortran both want LF.

**What CMake does:** checks `foofiles/types.foo` and `include/macros.in` and refuses
to configure if it finds CRLF.

**What you do:** clone from *inside* WSL. If you already have a CRLF checkout:

```bash
git config core.autocrlf input
git rm --cached -r .
git reset --hard
```

### 4. Running out of memory

Translation starts **one JVM per `.foo` file**, each wanting roughly 0.5–1 GB. WSL2 gives
its VM half the host's RAM by default. So `make -j$(nproc)` — fine on bare Linux with the
same core count — can meet the OOM killer here, and the build dies with a link or
"Killed" error that looks nothing like a memory problem.

**What CMake does:** prints a recommended `-j` in the configure log, computed as
`min(cpus, RAM_GB / 2)`:

```
-- WSL: 8 CPUs, 7 GB RAM visible to the VM
-- WSL: build with  make -j3  (translation runs one JVM per .foo file; higher -j risks the OOM killer)
```

**What you do:** use that number. To raise it, give the VM more memory — create
`%UserProfile%\.wslconfig` on the **Windows** side and run `wsl --shutdown`:

```ini
[wsl2]
memory=12GB
```

---

## `scripts/wsl_doctor.sh`

Run this before `cmake`. It reports the same four problems plus the ones CMake cannot
see — missing apt packages, unpopulated submodules, WSL 1 — in plain language, changes
nothing, and prints the exact command to fix each one.

```bash
scripts/wsl_doctor.sh                 # checks $PWD as the future build directory
scripts/wsl_doctor.sh ~/tonto-build   # or check a specific one
```

Exit status is 0 when you are ready to build, 1 when something blocking is wrong. Off
WSL it says so and exits immediately.

## WSL 1 vs WSL 2

Both work; WSL 2 is much faster at exactly the things this build does most (file I/O
and process creation — remember the JVM per file). If `wsl_doctor.sh` or the configure
log tells you that you are on WSL 1:

```powershell
wsl --set-version Ubuntu-24.04 2
```

## Escape hatches

| Option | Effect |
|--------|--------|
| `-DTONTO_WSL_STRICT=OFF` | Every check above warns instead of failing. The explanation is still printed. |
| `-DTONTO_WSL=OFF` | Disables WSL handling entirely, including the `PATH` sanitising. |
| `-DTONTO_WSL_KEEP_WINDOWS_PATH=ON` | Keeps `/mnt/*` on `PATH` while searching for tools. |
| `-DTONTO_WSL=ON` | Forces the WSL path on a non-WSL host (used by the tests). |

## Not covered

- **MPI.** MS-MPI interop from WSL is not tested and not guarded. A Linux MPI inside
  WSL (`sudo apt install libopenmpi-dev`) is the path that is likely to work.
- **Native Windows builds** (MSVC, or MinGW cross-compilation via
  `cmake/mingw_w64.cmake`) are a separate thing entirely and unaffected by any of this.

## How this is tested

Two layers, because they cost very different amounts — see `.github/workflows/ci-wsl.yml`:

- **`scripts/wsl_selftest.sh`** — every condition above is path- or file-shaped, so all
  of them can be simulated on an ordinary Linux box. The self-test drives
  `cmake/WSL.cmake` through a throwaway harness project and asserts, for each case, both
  the exit status and the message. It takes seconds and runs on **every push**.
- **A real WSL2 Ubuntu on a Windows runner** — configures, builds, and runs the short
  suite through `scripts/suite_report.py`, with the same loose gate as Linux CI so the
  numbers are directly comparable. It also re-checks the guards against a genuine drvfs
  mount and a genuine interop `PATH`. This runs weekly, on demand, and on any push that
  touches the WSL machinery.

See [`CI.md`](CI.md) for how to start either job by hand and how to read the result —
including the reason a manual run needs the workflow file on the default branch.
