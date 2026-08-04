#!/usr/bin/env bash
#
# Tonto WSL preflight.
#
# Run this inside WSL *before* cmake. It reports the same problems that
# cmake/WSL.cmake enforces at configure time, but earlier, in plain language, and
# including the ones CMake cannot see (missing apt packages, a checkout that is
# about to be built in the wrong place).
#
#   scripts/wsl_doctor.sh
#
# Exit 0 = ready to build; 1 = at least one blocking problem.
# Nothing is installed or changed; every fix is printed for you to run.

set -u

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

RED=''; YEL=''; GRN=''; DIM=''; OFF=''
if [ -t 1 ]; then
    RED=$'\033[31m'; YEL=$'\033[33m'; GRN=$'\033[32m'; DIM=$'\033[2m'; OFF=$'\033[0m'
fi

PROBLEMS=0
WARNINGS=0

ok()   { printf '  %sok%s    %s\n'   "$GRN" "$OFF" "$1"; }
warn() { printf '  %swarn%s  %s\n'   "$YEL" "$OFF" "$1"; WARNINGS=$((WARNINGS+1));
         [ $# -gt 1 ] && printf '        %sfix: %s%s\n' "$DIM" "$2" "$OFF"; return 0; }
bad()  { printf '  %sFAIL%s  %s\n'   "$RED" "$OFF" "$1"; PROBLEMS=$((PROBLEMS+1));
         [ $# -gt 1 ] && printf '        %sfix: %s%s\n' "$DIM" "$2" "$OFF"; return 0; }

echo "Tonto WSL preflight"
echo

# ---------------------------------------------------------------- environment
echo "environment"
KERNEL="$(cat /proc/sys/kernel/osrelease 2>/dev/null || echo unknown)"
# TONTO_WSL_DOCTOR_FORCE=1 runs the whole body on a non-WSL host, so the checks
# below are exercised by ordinary Linux CI rather than only on a Windows runner.
if [ "${TONTO_WSL_DOCTOR_FORCE:-0}" != "1" ] \
   && ! grep -qiE 'microsoft|wsl' <<<"$KERNEL" && [ -z "${WSL_DISTRO_NAME:-}" ]; then
    ok "not running under WSL (kernel $KERNEL) -- nothing here applies"
    echo
    echo "This script only has something to say inside WSL. Build normally; see README.md."
    exit 0
fi

if grep -q 'WSL2' <<<"$KERNEL" || [ -n "${WSL_INTEROP:-}" ]; then
    ok "WSL 2 (${WSL_DISTRO_NAME:-unknown distro}, kernel $KERNEL)"
else
    warn "WSL 1 (${WSL_DISTRO_NAME:-unknown distro}, kernel $KERNEL) -- filesystem and process creation are much slower, and the build starts one JVM per .foo file" \
         "wsl --set-version ${WSL_DISTRO_NAME:-<distro>} 2   (from a Windows prompt)"
fi

# ------------------------------------------------------------------- location
echo
echo "location"
case "$REPO" in
    /mnt/[a-zA-Z]/*)
        warn "the source tree is on a Windows drive ($REPO) -- this works (no Tonto path is illegal on NTFS) but every file read crosses the Windows/Linux boundary" \
             "git clone into your Linux home instead: cd ~ && git clone --recursive <url>" ;;
    *)  ok "source tree is on the Linux filesystem ($REPO)" ;;
esac

# The build tree is the one that really matters: it is written to constantly.
if [ -n "${1:-}" ]; then
    BUILDDIR="$1"
else
    BUILDDIR="$PWD"
fi
case "$BUILDDIR" in
    /mnt/[a-zA-Z]/*)
        bad "you are about to build in $BUILDDIR, which is on a Windows drive -- 10-50x slower, with an unreliable exec bit" \
            "mkdir -p ~/tonto-build && cd ~/tonto-build && cmake $REPO" ;;
    *)  ok "build directory ($BUILDDIR) is on the Linux filesystem" ;;
esac

# --------------------------------------------------------------- line endings
echo
echo "checkout"
CRLF=0
for f in "$REPO/foofiles/types.foo" "$REPO/include/macros.in"; do
    [ -f "$f" ] || continue
    if head -c 4096 "$f" | grep -q $'\r'; then CRLF=1; fi
done
if [ "$CRLF" = 1 ]; then
    bad "sources have Windows (CRLF) line endings -- the Foo translator and gfortran both need LF" \
        "git config core.autocrlf input && git rm --cached -r . && git reset --hard   (run from inside WSL)"
else
    ok "line endings are LF"
fi

EMPTY_SUBMODULES=""
while read -r sub; do
    [ -n "$sub" ] || continue
    if [ -z "$(ls -A "$REPO/$sub" 2>/dev/null)" ]; then
        EMPTY_SUBMODULES="$EMPTY_SUBMODULES $sub"
    fi
done < <(git -C "$REPO" config -f .gitmodules --get-regexp '^submodule\..*\.path$' 2>/dev/null | awk '{print $2}')
if [ -n "$EMPTY_SUBMODULES" ]; then
    # Not fatal: the ANTLR jar is downloaded by CMake, and system LAPACK is
    # preferred over the bundled copy. Only a release-static or COMPILE_LAPACK
    # build actually needs external/lapack-release checked out.
    warn "unpopulated submodule(s):$EMPTY_SUBMODULES -- fine for a normal release build, but needed for -DCMAKE_BUILD_TYPE=release-static or -DCOMPILE_LAPACK=ON" \
         "git submodule update --init --recursive"
else
    ok "git submodules are populated"
fi

# -------------------------------------------------------------------- toolchain
echo
echo "toolchain"
FC=""
for c in gfortran-14 gfortran-13 gfortran; do
    if command -v "$c" >/dev/null 2>&1; then FC="$c"; break; fi
done
if [ -n "$FC" ]; then
    ok "Fortran compiler: $FC ($($FC -dumpversion 2>/dev/null))"
else
    bad "no gfortran found" "sudo apt install gfortran-14"
fi

# Tonto is project(tonto LANGUAGES Fortran C), so a C compiler is required too.
# gfortran-14 pulls in gcc-14-base but NOT the gcc driver, and a bare WSL image
# has no C compiler at all -- so installing only gfortran gets you as far as
# "No CMAKE_C_COMPILER could be found". Desktop Ubuntu and CI runners ship one
# already, which is why this is a WSL-specific trap.
CC_FOUND=""
for c in gcc cc clang; do
    if command -v "$c" >/dev/null 2>&1; then CC_FOUND="$c"; break; fi
done
if [ -n "$CC_FOUND" ]; then
    ok "C compiler: $CC_FOUND ($($CC_FOUND -dumpversion 2>/dev/null))"
else
    bad "no C compiler -- cmake needs one for project(tonto LANGUAGES Fortran C), and gfortran does not provide it" \
        "sudo apt install gcc"
fi

# The interop PATH is the classic WSL trap: a Windows JDK shadows the Linux one,
# cannot read /home/... paths, and wants ';' rather than ':' between classpath
# entries -- which is exactly how the Foo translator is invoked.
for tool in java javac; do
    p="$(command -v $tool 2>/dev/null || true)"
    if [ -z "$p" ]; then
        bad "no $tool (needed to build the Foo->Fortran translator)" "sudo apt install default-jdk"
    elif [[ "$p" == /mnt/* || "$p" == *.exe ]]; then
        bad "$tool resolves to a WINDOWS executable ($p) -- it cannot read Linux paths and uses ';' as its classpath separator" \
            "sudo apt install default-jdk   (CMake also ignores /mnt/* when searching, so this only bites outside CMake)"
    else
        ok "$tool: $p"
    fi
done

for tool in cmake make python3 perl; do
    p="$(command -v $tool 2>/dev/null || true)"
    if [ -z "$p" ]; then
        bad "no $tool" "sudo apt install $tool"
    elif [[ "$p" == /mnt/* || "$p" == *.exe ]]; then
        bad "$tool is the Windows build ($p)" "sudo apt install $tool"
    else
        ok "$tool: $p"
    fi
done

if ! ldconfig -p 2>/dev/null | grep -q liblapack; then
    warn "no system LAPACK -- CMake will fall back to compiling the bundled reference LAPACK, which is slow to build and slower to run" \
         "sudo apt install libblas-dev liblapack-dev"
else
    ok "system BLAS/LAPACK present"
fi

WINPATH=$(tr ':' '\n' <<<"$PATH" | grep -c '^/mnt/[a-zA-Z]/' || true)
if [ "$WINPATH" -gt 0 ]; then
    ok "$WINPATH Windows directories on PATH (CMake ignores them; disable entirely if you prefer)"
    printf '        %soptional: put this in /etc/wsl.conf, then  wsl --shutdown%s\n' "$DIM" "$OFF"
    printf '        %s  [interop]%s\n'                "$DIM" "$OFF"
    printf '        %s  appendWindowsPath = false%s\n' "$DIM" "$OFF"
fi

# ------------------------------------------------------------------- resources
echo
echo "resources"
NCPU=$(nproc 2>/dev/null || echo 1)
MEMKB=$(awk '/^MemTotal:/{print $2}' /proc/meminfo 2>/dev/null || echo 0)
MEMGB=$((MEMKB/1048576))
JOBS=$NCPU
[ $((MEMGB/2)) -lt "$JOBS" ] && JOBS=$((MEMGB/2))
[ "$JOBS" -lt 1 ] && JOBS=1
ok "$NCPU CPUs, ${MEMGB} GB RAM visible to the VM"
# Translation runs `java` once per .foo file, ~0.5-1 GB each; WSL2 gives its VM
# half the host's RAM by default, so nproc alone over-commits.
printf '        %sbuild with: make -j%s%s\n' "$DIM" "$JOBS" "$OFF"
if [ "$JOBS" -lt "$NCPU" ]; then
    printf '        %scapped by RAM, not CPUs. To raise it, put this in %%UserProfile%%\\.wslconfig%s\n' "$DIM" "$OFF"
    printf '        %s(Windows side), then  wsl --shutdown:%s\n' "$DIM" "$OFF"
    printf '        %s  [wsl2]%s\n'       "$DIM" "$OFF"
    printf '        %s  memory=8GB%s\n'   "$DIM" "$OFF"
fi

# ---------------------------------------------------------------------- verdict
echo
if [ "$PROBLEMS" -gt 0 ]; then
    printf '%s%d blocking problem(s)%s, %d warning(s). Fix the FAIL lines above, then:\n' \
           "$RED" "$PROBLEMS" "$OFF" "$WARNINGS"
    echo "    cmake -B ~/tonto-build -S $REPO -DCMAKE_Fortran_COMPILER=gfortran-14 -DCMAKE_BUILD_TYPE=release"
    exit 1
fi
printf '%sReady to build%s (%d warning(s)):\n' "$GRN" "$OFF" "$WARNINGS"
echo "    cmake -B ~/tonto-build -S $REPO -DCMAKE_Fortran_COMPILER=${FC:-gfortran-14} -DCMAKE_BUILD_TYPE=release"
echo "    cmake --build ~/tonto-build -- -j$JOBS"
echo "    cd ~/tonto-build && ctest -L short"
exit 0
