#!/usr/bin/env bash
#
# Self-test for scripts/rgbi_doctor.sh.
#
# A doctor that always says "you are fine" is worse than no doctor, and on a
# developer machine with everything installed that is exactly what it will say.
# So this drives it against synthetic environments and asserts that each check
# FIRES: the tool genuinely absent, or present-but-unrunnable.
#
# Absence is simulated honestly -- by building a minimal PATH containing only
# the handful of coreutils the doctor itself needs, plus whichever tools the
# case under test wants present. Nothing is stubbed inside the doctor, so there
# is no test-only code path that could diverge from the real one.
#
# Usage:  scripts/rgbi_selftest.sh [-v]
# Exit:   0 = all cases passed

set -u

VERBOSE=0
[ "${1:-}" = "-v" ] && VERBOSE=1

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DOCTOR="$REPO/scripts/rgbi_doctor.sh"
# The synthetic PATHs below deliberately omit almost everything, bash included,
# so the interpreter must be named absolutely rather than looked up.
BASH_BIN="$(command -v bash)"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

PASS=0
FAIL=0

# The doctor's own dependencies. Without these on PATH it cannot run at all,
# and every case would fail for the wrong reason.
BASE_TOOLS="uname grep sed awk tr head cat"

# make_path <tool> ... -- a PATH with the base tools plus the named ones
make_path() {
    local dir="$WORK/bin-$RANDOM$RANDOM" t p
    mkdir -p "$dir"
    for t in $BASE_TOOLS "$@"; do
        p="$(command -v "$t" 2>/dev/null)" || continue
        ln -sf "$p" "$dir/$t"
    done
    printf '%s' "$dir"
}

# check <name> <expected-exit: 0|nonzero> <expected-substring|-> <PATH> [doctor args...]
check() {
    local name="$1" want_exit="$2" want_text="$3" path="$4"; shift 4
    local out rc
    out="$(PATH="$path" HOME="$WORK/fakehome" "$BASH_BIN" "$DOCTOR" "$@" 2>&1)"; rc=$?

    local good=1
    [ "$want_exit" = "0" ]       && [ "$rc" -ne 0 ] && good=0
    [ "$want_exit" = "nonzero" ] && [ "$rc" -eq 0 ] && good=0
    if [ "$want_text" != "-" ] && ! grep -qF -- "$want_text" <<<"$out"; then good=0; fi

    if [ "$good" = 1 ]; then
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

mkdir -p "$WORK/fakehome"

echo "scripts/rgbi_doctor.sh self-test"
echo

# ------------------------------------------------------------ missing tools
echo "missing tools are reported, and block"
BARE="$(make_path)"
check "no pdflatex      -> FAIL"      nonzero "no pdflatex"                "$BARE"
check "no ghostscript   -> FAIL"      nonzero "no ghostscript"             "$BARE"
check "no kpsewhich     -> FAIL"      nonzero "no kpsewhich"               "$BARE"
check "no obabel        -> FAIL"      nonzero "no obabel"                  "$BARE"
check "no mol2chemfig   -> FAIL"      nonzero "no mol2chemfig"             "$BARE"
check "the gs message says WHY"       nonzero "pdfcrop uses it"            "$BARE"
check "the fix is a runnable command" nonzero "pipx install mol2chemfigPy3" "$BARE"

# ------------------------------------------------------------- the two tiers
echo
echo "the two tiers: dial diagrams do not need the arcane software"
FULL_TEX="$(make_path pdflatex pdfcrop gs kpsewhich)"
check "--dials-only passes with no obabel/mol2chemfig" 0 "Ready to draw dial diagrams" "$FULL_TEX" --dials-only
check "without --dials-only the same box FAILS"  nonzero "no obabel"                   "$FULL_TEX"
check "and it says how to lower the bar"         nonzero "--dials-only"                "$FULL_TEX"

# ------------------------------------------- installed but unrunnable (the point)
echo
echo "installed-but-unrunnable is caught -- the case command -v cannot see"
BROKEN="$(make_path pdflatex pdfcrop gs kpsewhich obabel)"
printf '#!/nonexistent/python\nprint("never runs")\n' > "$BROKEN/mol2chemfig"
chmod +x "$BROKEN/mol2chemfig"
check "dead interpreter -> FAIL, not ok"  nonzero "CANNOT RUN"        "$BROKEN"
check "and it names the real fix"         nonzero "pipx reinstall-all" "$BROKEN"

# A mol2chemfig that runs but whose environment lacks Indigo.
NOINDIGO="$(make_path pdflatex pdfcrop gs kpsewhich obabel)"
cat > "$NOINDIGO/mol2chemfig" <<EOF
#!$(command -v python3)
import sys
print("mol2chemfig version 1.5.12")
EOF
chmod +x "$NOINDIGO/mol2chemfig"
check "runs but no Indigo -> FAIL"        nonzero "Indigo is not importable" "$NOINDIGO"

# -------------------------------------------------------------- templates
echo
echo "templates"
check "templates found in the checkout"   0 "template directory"  "$(make_path pdflatex pdfcrop gs kpsewhich obabel mol2chemfig)"
check "--print-template-dir prints a path" 0 "rgbi-scripts"        "$BARE" --print-template-dir

# A copy of the doctor with no repository, no ~/bin, and no override: the
# search must fail rather than quietly pointing somewhere useless.
ORPHAN="$WORK/orphan"; mkdir -p "$ORPHAN"
cp "$DOCTOR" "$ORPHAN/rgbi_doctor.sh"
OUT="$(PATH="$BARE" HOME="$WORK/fakehome" TONTO_RGBI_SCRIPT_DIRECTORY= "$BASH_BIN" "$ORPHAN/rgbi_doctor.sh" 2>&1)"
if grep -qF "cannot find the LaTeX templates" <<<"$OUT"; then
    PASS=$((PASS+1)); printf '  ok    %s\n' "no templates anywhere -> FAIL"
else
    FAIL=$((FAIL+1)); printf '  FAIL  %s\n' "no templates anywhere -> FAIL"
    sed 's/^/        | /' <<<"$OUT"
fi

# An override must win, and must be rejected if it does not hold the templates.
OUT="$(PATH="$BARE" HOME="$WORK/fakehome" TONTO_RGBI_SCRIPT_DIRECTORY="$WORK/nothing-here" \
       "$BASH_BIN" "$ORPHAN/rgbi_doctor.sh" --print-template-dir 2>&1)"; rc=$?
if [ "$rc" -ne 0 ]; then
    PASS=$((PASS+1)); printf '  ok    %s\n' "an override without the templates is not accepted"
else
    FAIL=$((FAIL+1)); printf '  FAIL  %s (exit %s, said %q)\n' "an override without the templates is not accepted" "$rc" "$OUT"
fi

# ...and when it is ignored in favour of a directory that DOES hold them, that
# has to be said out loud. Silently drawing with templates the user did not
# choose is the failure mode this whole restoration is about.
OUT="$(PATH="$BARE" TONTO_RGBI_SCRIPT_DIRECTORY="$WORK/nothing-here" \
       "$BASH_BIN" "$DOCTOR" --dials-only 2>&1)"
if grep -qF "was IGNORED" <<<"$OUT"; then
    PASS=$((PASS+1)); printf '  ok    %s\n' "a useless override is reported, not silently dropped"
else
    FAIL=$((FAIL+1)); printf '  FAIL  %s\n' "a useless override is reported, not silently dropped"
    sed 's/^/        | /' <<<"$OUT"
fi

OUT="$(PATH="$BARE" TONTO_RGBI_SCRIPT_DIRECTORY="$REPO/rgbi-scripts" "$BASH_BIN" "$ORPHAN/rgbi_doctor.sh" --print-template-dir 2>&1)"
if [ "$OUT" = "$REPO/rgbi-scripts" ]; then
    PASS=$((PASS+1)); printf '  ok    %s\n' "TONTO_RGBI_SCRIPT_DIRECTORY is honoured"
else
    FAIL=$((FAIL+1)); printf '  FAIL  %s (got %q)\n' "TONTO_RGBI_SCRIPT_DIRECTORY is honoured" "$OUT"
fi

# ------------------------------------------------------------------ quiet
echo
echo "--quiet, as the scripts use it"
# --quiet drops the "ok" lines, not the warnings: a preflight that hides a
# warning is only half a preflight.
GOOD="$(make_path pdflatex pdfcrop gs kpsewhich obabel mol2chemfig)"
OUT="$(PATH="$GOOD" "$BASH_BIN" "$DOCTOR" --quiet 2>&1)"; rc=$?
if [ "$rc" -eq 0 ] && ! grep -qE '^  ok ' <<<"$OUT"; then
    PASS=$((PASS+1)); printf '  ok    %s\n' "no 'ok' chatter when everything is fine"
else
    FAIL=$((FAIL+1)); printf '  FAIL  %s (exit %s)\n' "no 'ok' chatter when everything is fine" "$rc"
    sed 's/^/        | /' <<<"$OUT"
fi
check "but still speaks up on a problem" nonzero "no pdflatex" "$BARE" --quiet

echo
if [ "$FAIL" -eq 0 ]; then
    echo "PASSED  $PASS/$PASS"
    exit 0
else
    echo "FAILED  $FAIL of $((PASS+FAIL))"
    exit 1
fi
