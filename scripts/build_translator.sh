#!/bin/bash
# Build the ANTLR4-based Foo->Fortran translator (foogrammar/FooToFortran.java).
#
#   scripts/build_translator.sh            # generate parser + compile translator
#   scripts/build_translator.sh <file.foo> # also run it on one module
#
# Outputs land in antlr4-release/ ; compare against release/ (foo.pl reference).
set -euo pipefail
cd "$(dirname "$0")/.."

JAR="${ANTLR_JAR:-/usr/local/lib/antlr-4.13.2-complete.jar}"
GEN=build/translator/gen
CLS=build/translator/classes

echo "[1/2] generate parser"
rm -rf "$GEN"; mkdir -p "$GEN"
( cd foogrammar && java -Xmx500M -cp "$JAR" org.antlr.v4.Tool -visitor -o "../$GEN" Foo.g4 )

echo "[2/2] compile translator"
rm -rf "$CLS"; mkdir -p "$CLS"
javac -cp "$JAR" -d "$CLS" "$GEN"/*.java foogrammar/FooToFortran.java

if [ "${1:-}" != "" ]; then
  echo "run: $1"
  java -cp "$JAR:$CLS" FooToFortran \
    --types foofiles/types.foo --foo "$1" --out-dir antlr4-release
fi
echo "done"
