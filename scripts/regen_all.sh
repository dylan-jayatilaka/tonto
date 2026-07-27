#!/bin/bash
# Regenerate ALL translator output (every foofiles/ + runfiles/run_*.foo) into a
# target dir, in ONE JVM (batch mode). Used for phase-boundary zero-diff checks in
# the call-resolution work. Assumes the translator is already built
# (scripts/build_translator.sh).
#
#   scripts/regen_all.sh <out-dir>
#
# Compare with:  diff -rq release/ <out-dir>   (or against a frozen snapshot).
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="${1:?usage: regen_all.sh <out-dir>}"
JAR="${ANTLR_JAR:-$PWD/external/antlr-4.13.2-complete.jar}"
CLS=build/translator/classes

[ -d "$CLS" ] || { echo "translator not built; run scripts/build_translator.sh" >&2; exit 1; }

rm -rf "$OUT"; mkdir -p "$OUT"
RUNLIST="$(mktemp)"; ls runfiles/run_*.foo > "$RUNLIST"
java -cp "$JAR:$CLS" FooToFortran --types foofiles/types.foo \
     --foo-dir foofiles --foo-list "$RUNLIST" --out-dir "$OUT"
rm -f "$RUNLIST"
echo "regenerated into $OUT"
