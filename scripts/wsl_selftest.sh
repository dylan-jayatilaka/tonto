#!/usr/bin/env bash
#
# Self-test for cmake/WSL.cmake.
#
# The WSL guards exist to catch four environment problems (Windows JDK on the
# interop PATH, a build tree on /mnt/c, CRLF sources, WSL1) that are expensive to
# hit and confusing to diagnose. Guards that are themselves wrong are worse than
# no guards, so they need a test -- but standing up a Windows runner to check
# them is slow and only available in CI.
#
# Every one of those conditions is path- or file-shaped, so all of them can be
# simulated on an ordinary Linux box. This script drives cmake/WSL.cmake through
# a minimal harness project (nothing of Tonto is built) and asserts, for each
# case, both the exit status and the message. It runs in a couple of seconds and
# needs only cmake and a JDK.
#
# Usage:  scripts/wsl_selftest.sh [-v]
# Exit:   0 = all cases passed

set -u

VERBOSE=0
[ "${1:-}" = "-v" ] && VERBOSE=1

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CMAKE_DIR="$REPO/cmake"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

PASS=0
FAIL=0

if ! command -v javac >/dev/null 2>&1; then
    echo "SKIP: no javac on PATH -- cmake/WSL.cmake's 'is there a Linux JDK' check"
    echo "      would fire in every case and mask the others."
    exit 0
fi

# A harness project that exercises WSL.cmake and nothing else. It deliberately
# does NOT call find_package(Java): that would overwrite the Java_JAVA_EXECUTABLE
# the Windows-JDK case needs to inject.
make_harness() {          # $1 = directory, $2 = "crlf" to plant a CRLF source
    local dir="$1"
    mkdir -p "$dir/foofiles" "$dir/include"
    cat > "$dir/CMakeLists.txt" <<EOF
cmake_minimum_required(VERSION 3.20)
project(wsl_selftest NONE)
set(CMAKE_MODULE_PATH \${CMAKE_MODULE_PATH} $CMAKE_DIR)
include(WSL)
tonto_wsl_preflight()
tonto_wsl_finalize()
message(STATUS "SELFTEST-REACHED-END")
EOF
    if [ "${2:-}" = "crlf" ]; then
        printf 'module TYPES\r\n   contains\r\nend\r\n' > "$dir/foofiles/types.foo"
    else
        printf 'module TYPES\n   contains\nend\n' > "$dir/foofiles/types.foo"
    fi
}

# check <name> <expected-exit: 0|nonzero> <expected-substring|-> <cmake args...>
check() {
    local name="$1" want_exit="$2" want_text="$3"; shift 3
    local out rc
    out="$(cmake -S "$SRC" -B "$WORK/build-$RANDOM$RANDOM" "$@" 2>&1)"; rc=$?

    local ok=1
    if [ "$want_exit" = "0" ] && [ "$rc" -ne 0 ]; then ok=0; fi
    if [ "$want_exit" = "nonzero" ] && [ "$rc" -eq 0 ]; then ok=0; fi
    if [ "$want_text" != "-" ] && ! grep -qF -- "$want_text" <<<"$out"; then ok=0; fi

    if [ "$ok" = 1 ]; then
        PASS=$((PASS+1)); printf '  ok    %s\n' "$name"
        [ "$VERBOSE" = 1 ] && sed 's/^/          /' <<<"$out"
    else
        FAIL=$((FAIL+1))
        printf '  FAIL  %s\n' "$name"
        printf '        expected exit %s and text %q; got exit %s\n' "$want_exit" "$want_text" "$rc"
        sed 's/^/        | /' <<<"$out"
    fi
    return 0
}

# not_text <name> <forbidden-substring> <cmake args...>
not_text() {
    local name="$1" bad="$2"; shift 2
    local out rc
    out="$(cmake -S "$SRC" -B "$WORK/build-$RANDOM$RANDOM" "$@" 2>&1)"; rc=$?
    if [ "$rc" -eq 0 ] && ! grep -qF -- "$bad" <<<"$out"; then
        PASS=$((PASS+1)); printf '  ok    %s\n' "$name"
    else
        FAIL=$((FAIL+1)); printf '  FAIL  %s (exit %s; %q present)\n' "$name" "$rc" "$bad"
        sed 's/^/        | /' <<<"$out"
    fi
    return 0
}

echo "cmake/WSL.cmake self-test"
echo

SRC="$WORK/plain"; make_harness "$SRC"

echo "detection"
not_text "host is not WSL -> module stays silent"        "WSL:"
not_text "TONTO_WSL=OFF   -> module stays silent"        "WSL:" -DTONTO_WSL=OFF
check    "TONTO_WSL=ON    -> reports and continues"  0 "WSL: detected" -DTONTO_WSL=ON
check    "TONTO_WSL=ON    -> advises a -j value"     0 "build with  make -j" -DTONTO_WSL=ON

echo
echo "guards (strict, the default)"
check "build tree on /mnt/c is rejected" nonzero "The build tree is on a Windows drive" \
      -DTONTO_WSL=ON -DTONTO_WSL_TEST_BINARY_DIR=/mnt/c/Users/me/build
check "a Windows JDK is rejected" nonzero "is a *Windows* JDK" \
      -DTONTO_WSL=ON -DJava_JAVA_EXECUTABLE=/mnt/c/jdk/bin/java.exe
check "a .exe JDK outside /mnt is rejected too" nonzero "is a *Windows* JDK" \
      -DTONTO_WSL=ON -DJava_JAVAC_EXECUTABLE=C:/jdk/bin/javac.exe
check "the error names the override" nonzero "TONTO_WSL_STRICT=OFF" \
      -DTONTO_WSL=ON -DTONTO_WSL_TEST_BINARY_DIR=/mnt/c/x

SRC="$WORK/crlf"; make_harness "$SRC" crlf
check "CRLF sources are rejected" nonzero "has Windows (CRLF) line endings" -DTONTO_WSL=ON
SRC="$WORK/plain"

echo
echo "escape hatch (TONTO_WSL_STRICT=OFF)"
check "/mnt/c build tree warns but proceeds" 0 "SELFTEST-REACHED-END" \
      -DTONTO_WSL=ON -DTONTO_WSL_STRICT=OFF -DTONTO_WSL_TEST_BINARY_DIR=/mnt/c/x
check "the warning still explains the problem" 0 "The build tree is on a Windows drive" \
      -DTONTO_WSL=ON -DTONTO_WSL_STRICT=OFF -DTONTO_WSL_TEST_BINARY_DIR=/mnt/c/x

echo
echo "PATH sanitisation"
# Simulate the interop PATH: Windows directories appended to the Linux one. The
# guard must drop exactly those and leave the Linux entries alone -- so the JDK
# probe, which runs after the strip, must still succeed.
(
    export PATH="$PATH:/mnt/c/Windows/system32:/mnt/c/Program Files/Java/jdk/bin"
    check "two Windows PATH entries are dropped" 0 "dropped 2 Windows PATH entries" -DTONTO_WSL=ON
    check "Linux PATH entries survive the strip"  0 "SELFTEST-REACHED-END"           -DTONTO_WSL=ON
    check "opt-out keeps them"                    0 "Windows PATH entries KEPT" \
          -DTONTO_WSL=ON -DTONTO_WSL_KEEP_WINDOWS_PATH=ON
    exit $FAIL
)
SUBSHELL_FAIL=$?
FAIL=$((FAIL + SUBSHELL_FAIL))
PASS=$((PASS + 3 - SUBSHELL_FAIL))

echo
if [ "$FAIL" -eq 0 ]; then
    echo "PASSED  $PASS/$PASS"
    exit 0
else
    echo "FAILED  $FAIL of $((PASS+FAIL))"
    exit 1
fi
