#!/usr/bin/env bash
#
# RGBI picture-tool preflight.
#
# Run this before make-rgbi-pic / make-rgbi-dials. It reports what is missing,
# in plain language, with the command that fixes it -- instead of letting the
# pipeline fail obscurely four programs later.
#
#   scripts/rgbi_doctor.sh              full check, both pictures
#   scripts/rgbi_doctor.sh --dials-only only what dial diagrams need
#   scripts/rgbi_doctor.sh --quiet      only problems (used as a preflight)
#   scripts/rgbi_doctor.sh --print-template-dir
#
# Exit 0 = you can draw; 1 = at least one blocking problem.
# Nothing is installed or changed; every fix is printed for you to run.
#
# WHY IT EXECUTES THINGS RATHER THAN LOOKING FOR THEM
#
# On 2026-08-09 this machine had a mol2chemfig that `command -v` found happily
# and that could not run: pipx had pinned its virtualenv to a python the OS had
# since removed. The pipeline's only symptom was a picture that did not appear.
# So every check below that can be run, is run.
#
# See docs/INSTALLING_RGBI.md (how to install) and docs/RUNNING_RGBI.md (what
# the pipeline is, and which half needs what).

set -u

QUIET=0
DIALS_ONLY=0

while [ $# -gt 0 ]; do
    case "$1" in
        --quiet|-q)       QUIET=1 ;;
        --dials-only)     DIALS_ONLY=1 ;;
        --print-template-dir) PRINT_TEMPLATE_DIR=1 ;;
        --help|-h)
            sed -n '2,26p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *)  printf 'rgbi_doctor.sh: unknown option %s (try --help)\n' "$1" >&2; exit 2 ;;
    esac
    shift
done

# Deliberately not `dirname`: this script has to be able to report a PATH so
# broken that coreutils are missing from it, which is exactly when a user needs
# it most. Parameter expansion cannot fail that way.
_src="${BASH_SOURCE[0]}"
case "$_src" in */*) _dir="${_src%/*}" ;; *) _dir="." ;; esac
HERE="$(cd "$_dir" && pwd)"

RED=''; YEL=''; GRN=''; DIM=''; OFF=''
if [ -t 1 ]; then
    RED=$'\033[31m'; YEL=$'\033[33m'; GRN=$'\033[32m'; DIM=$'\033[2m'; OFF=$'\033[0m'
fi

PROBLEMS=0
WARNINGS=0

ok()   { [ "$QUIET" = 1 ] && return 0
         printf '  %sok%s    %s\n'   "$GRN" "$OFF" "$1"; return 0; }
warn() { printf '  %swarn%s  %s\n'   "$YEL" "$OFF" "$1"; WARNINGS=$((WARNINGS+1));
         [ $# -gt 1 ] && printf '        %sfix: %s%s\n' "$DIM" "$2" "$OFF"; return 0; }
bad()  { printf '  %sFAIL%s  %s\n'   "$RED" "$OFF" "$1"; PROBLEMS=$((PROBLEMS+1));
         [ $# -gt 1 ] && printf '        %sfix: %s%s\n' "$DIM" "$2" "$OFF"; return 0; }
say()  { [ "$QUIET" = 1 ] || printf '%s\n' "$1"; }

# A structure-tier problem is only blocking if you asked for the structure
# picture. With --dials-only it is a warning: the dial diagrams do not need
# Open Babel, Indigo or mol2chemfig at all.
tier2() { if [ "$DIALS_ONLY" = 1 ]; then warn "$@"; else bad "$@"; fi }

# ---------------------------------------------------------------- platform
case "$(uname -s)" in
    Darwin) PLATFORM=macos ;;
    Linux)  if grep -qiE 'microsoft|wsl' /proc/sys/kernel/osrelease 2>/dev/null \
               || [ -n "${WSL_DISTRO_NAME:-}" ]; then PLATFORM=wsl; else PLATFORM=linux; fi ;;
    *)      PLATFORM=other ;;
esac

# Package-manager-appropriate fix hints, so the advice is runnable rather than
# merely correct.
case "$PLATFORM" in
    macos) PKG_TEX="brew install --cask mactex-no-gui"
           PKG_BABEL="brew install open-babel"
           PKG_GS="brew install ghostscript"
           PKG_PIPX="brew install pipx" ;;
    *)     PKG_TEX="sudo apt install texlive-latex-base texlive-latex-recommended texlive-pictures texlive-extra-utils"
           PKG_BABEL="sudo apt install openbabel"
           PKG_GS="sudo apt install ghostscript"
           PKG_PIPX="sudo apt install pipx" ;;
esac

# ------------------------------------------------------- template location
# THE single implementation of this search. make-rgbi-pic and make-rgbi-dials
# ask for it with --print-template-dir rather than keeping a second copy that
# can drift.
TEMPLATES="cf-pastebin.tex mol2chemfig.sty rgbi-mol-structure.tex
           rgbi-dial-header.tex rgbi-dial-diagrams.tex"

has_all_templates() {
    local d="$1" f
    [ -d "$d" ] || return 1
    for f in $TEMPLATES; do [ -r "$d/$f" ] || return 1; done
    return 0
}

find_template_dir() {
    local c
    # Only three places are real, and they are the three ways this can be
    # deployed: a git checkout (templates beside the scripts), an installed
    # tree (no repository at all -- <prefix>/bin/make-rgbi-pic with the
    # templates under <prefix>/share/tonto), and the historical ~/bin, which
    # keeps working on purpose. Plus an explicit override for everything else.
    for c in "${TONTO_RGBI_SCRIPT_DIRECTORY:-}" \
             "$HERE/../rgbi-scripts" \
             "$HERE/../share/tonto/rgbi-scripts" \
             "$HOME/bin"; do
        [ -n "$c" ] || continue
        if has_all_templates "$c"; then (cd "$c" && pwd); return 0; fi
    done
    return 1
}

if [ "${PRINT_TEMPLATE_DIR:-0}" = 1 ]; then
    find_template_dir || exit 1
    exit 0
fi

# ==========================================================================
say "RGBI picture-tool preflight"
say ""

say "platform"
case "$PLATFORM" in
    linux) ok "Linux -- supported and tested (scripts/docker/rgbi.Dockerfile builds this from scratch in CI)" ;;
    wsl)   ok "WSL (${WSL_DISTRO_NAME:-unknown distro}) -- supported"
           printf '        %salso run: scripts/wsl_doctor.sh -- the Windows/Linux boundary traps are checked there%s\n' "$DIM" "$OFF"
           printf '        %sand keep the job directory OFF /mnt/c: pdflatex runs four times per picture%s\n' "$DIM" "$OFF" ;;
    macos) ok "macOS -- run through by hand on Apple silicon (2026-08-24) and probed weekly by ci-rgbi-macos.yml" ;;
    *)     warn "unrecognised platform $(uname -s) -- the checks still apply, the fix hints assume apt" ;;
esac

# ------------------------------------------------------------------ tonto
say ""
say "tonto"
if command -v rgbi >/dev/null 2>&1; then
    ok "rgbi: $(command -v rgbi)"
elif [ -x "$HERE/../release/rgbi" ]; then
    ok "rgbi: $HERE/../release/rgbi (not on PATH)"
else
    warn "no rgbi on PATH -- you need it to PRODUCE the .tex fragments, but not to draw ones you already have" \
         "build it: see docs/DOCUMENTATION.md, then add <prefix>/bin to PATH"
fi

# ------------------------------------------------------------------- LaTeX
# Tier 1: everything the dial diagrams need, and nothing else.
say ""
say "LaTeX (needed for BOTH pictures)"
for tool in pdflatex pdfcrop; do
    if command -v "$tool" >/dev/null 2>&1; then
        ok "$tool: $(command -v $tool)"
    else
        bad "no $tool" "$PKG_TEX"
    fi
done

# pdfcrop shells out to ghostscript. Without gs every picture dies at the very
# last step, after all the work is done -- and this was documented nowhere.
if command -v gs >/dev/null 2>&1; then
    ok "ghostscript: $(gs --version 2>/dev/null) -- pdfcrop needs it"
else
    bad "no ghostscript -- pdfcrop uses it, so cropping fails after everything else has succeeded" "$PKG_GS"
fi

if command -v kpsewhich >/dev/null 2>&1; then
    # ifmtarg and twoopt are here because rgbi-scripts/mol2chemfig.sty does
    # \RequirePackage{xcolor, twoopt, ifmtarg, tikz}, and BOTH templates load
    # mol2chemfig. They were missing from this list, so a machine without them
    # got a green doctor and then `! LaTeX Error: File 'ifmtarg.sty' not
    # found.` at draw time -- which is exactly what the macOS runner did on
    # 2026-08-24. A preflight that misses a hard requirement is worse than none.
    for sty in chemfig tikz xcolor longtable graphicx ifthen geometry ifmtarg twoopt; do
        if kpsewhich "$sty.sty" >/dev/null 2>&1; then
            ok "$sty.sty"
        else
            bad "LaTeX package missing: $sty.sty" "$PKG_TEX"
        fi
    done
else
    bad "no kpsewhich -- cannot tell which LaTeX packages you have" "$PKG_TEX"
fi

# -------------------------------------------------------- structure picture
# Tier 2: only the labelled molecular structure needs any of this.
say ""
say "structure picture (dial diagrams do NOT need any of this)"

if command -v obabel >/dev/null 2>&1; then
    ok "obabel: $(command -v obabel)"
    FMT="$(obabel -L formats 2>&1 || true)"
    for f in fchk molden; do
        if grep -qiE "^ *$f " <<<"$FMT"; then
            ok "obabel reads .$f"
        else
            tier2 "obabel cannot read .$f -- rgbi writes those" "$PKG_BABEL"
        fi
    done
else
    tier2 "no obabel (Open Babel) -- generates the 2D layout" "$PKG_BABEL"
fi

# The check that matters. `command -v mol2chemfig` succeeds on a machine where
# mol2chemfig cannot run, because pipx pins each app to the python of the day
# and an OS upgrade deletes that python out from under it. So: run it.
if command -v mol2chemfig >/dev/null 2>&1; then
    M2C_OUT="$(mol2chemfig --version 2>&1)"; M2C_RC=$?
    if [ "$M2C_RC" -ne 0 ] || grep -q 'bad interpreter' <<<"$M2C_OUT"; then
        tier2 "mol2chemfig is INSTALLED BUT CANNOT RUN: $(tr '\n' ' ' <<<"$M2C_OUT" | head -c 120)" \
              "pipx reinstall-all   (its virtualenv points at a python your OS has removed)"
    else
        ok "mol2chemfig runs: $(grep -o 'version.*' <<<"$M2C_OUT" | head -1)"
        # Indigo lives inside mol2chemfig's own venv -- importing it from the
        # system python proves nothing, so ask the venv's interpreter.
        # Find the venv's python by RESOLVING THE APP, not by reading its
        # shebang. pipx installs the app as a symlink into its own venv, and
        # the file itself can be a /bin/sh polyglot shim whose first line is
        # "#!/bin/sh" -- so shebang-parsing yields /bin/sh, `/bin/sh -c
        # 'import indigo'` fails, and the doctor reports Indigo missing on a
        # machine where it imports perfectly. Measured on macOS 26.5 with
        # pipx 1.8 (2026-08-24). The real interpreter is on the shim's SECOND
        # line and its path contains spaces on macOS ("Application Support"),
        # which is a second reason not to parse it.
        M2C_APP="$(command -v mol2chemfig)"
        if command -v readlink >/dev/null 2>&1; then
            while [ -L "$M2C_APP" ]; do      # no readlink -f on macOS
                _t="$(readlink "$M2C_APP")"
                case "$_t" in
                    /*) M2C_APP="$_t" ;;
                     *) M2C_APP="${M2C_APP%/*}/$_t" ;;
                esac
            done
        fi
        M2C_PY="${M2C_APP%/*}/python"
        # Fall back to the shebang for a plain script that is not in a venv.
        [ -x "$M2C_PY" ] || \
            M2C_PY="$(sed -n '1s/^#!//p' "$(command -v mol2chemfig)" 2>/dev/null | awk '{print $1}')"
        if [ -x "${M2C_PY:-}" ]; then
            if "$M2C_PY" -c 'import indigo' >/dev/null 2>&1; then
                ok "Indigo importable (it is a dependency of mol2chemfigPy3, not a separate install)"
            else
                tier2 "mol2chemfig runs but Indigo is not importable in its environment" \
                      "pipx reinstall mol2chemfigPy3"
            fi
        fi
    fi
else
    tier2 "no mol2chemfig -- converts the molecule to chemfig markup" \
          "$PKG_PIPX && pipx install mol2chemfigPy3   (brings Indigo with it)"
fi

# ---------------------------------------------------------------- templates
say ""
say "templates"
# An override that does not hold the templates is a mistake worth naming.
# Falling through to another directory in silence is how you end up drawing
# with templates you did not choose. (Checked here, not inside
# find_template_dir: that runs in a command substitution, so anything it set
# would die with the subshell.)
if [ -n "${TONTO_RGBI_SCRIPT_DIRECTORY:-}" ] \
   && ! has_all_templates "$TONTO_RGBI_SCRIPT_DIRECTORY"; then
    BAD_OVERRIDE="$TONTO_RGBI_SCRIPT_DIRECTORY"
fi
if TDIR="$(find_template_dir)"; then
    ok "template directory: $TDIR"
    if [ -n "${BAD_OVERRIDE:-}" ]; then
        warn "TONTO_RGBI_SCRIPT_DIRECTORY is set to $BAD_OVERRIDE, which does not hold the templates -- it was IGNORED" \
             "unset it, or point it at a directory containing $(echo $TEMPLATES | tr '\n' ' ')"
    fi
    case "$TDIR" in
        "$HOME/bin") warn "these are the legacy copies in ~/bin -- they can drift from the repository" \
                          "install Tonto, or set TONTO_RGBI_SCRIPT_DIRECTORY=<repo>/rgbi-scripts" ;;
    esac
else
    bad "cannot find the LaTeX templates ($(echo $TEMPLATES | tr '\n' ' '))" \
        "set TONTO_RGBI_SCRIPT_DIRECTORY=<repo>/rgbi-scripts, or install Tonto"
fi

# ------------------------------------------------------------------ verdict
say ""
if [ "$PROBLEMS" -gt 0 ]; then
    printf '%s%d blocking problem(s)%s, %d warning(s). Fix the FAIL lines above.\n' \
           "$RED" "$PROBLEMS" "$OFF" "$WARNINGS"
    [ "$DIALS_ONLY" = 1 ] || printf '%sIf you only want dial diagrams, re-run with --dials-only: they need far less.%s\n' "$DIM" "$OFF"
    exit 1
fi
if [ "$QUIET" != 1 ]; then
    if [ "$DIALS_ONLY" = 1 ]; then
        printf '%sReady to draw dial diagrams%s (%d warning(s)).\n' "$GRN" "$OFF" "$WARNINGS"
    else
        printf '%sReady to draw both pictures%s (%d warning(s)).\n' "$GRN" "$OFF" "$WARNINGS"
    fi
fi
exit 0
