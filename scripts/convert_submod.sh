#!/bin/bash
# Convert submodule-qualified calls to bare dot form in the given .foo files.
# Handles (in order, TYPE-prefixed first so the .SUBMOD: inside isn't matched early):
#   CLASS.SUBMOD::proc  -> .::proc     (type+submodule, non-generic; strip both)
#   CLASS.SUBMOD:proc   -> .proc       (type+submodule, generic;     strip both)
#   .SUBMOD::proc       -> .::proc     (self-submodule, non-generic)
#   .SUBMOD:proc        -> .proc       (self-submodule, generic)
# Leaves TYPE:proc (module-qualified, non-submodule) untouched for a later pass.
#
#   convert_submod.sh CLASS SUBMOD1 SUBMOD2 ... -- file1.foo file2.foo ...
set -euo pipefail
CLASS="$1"; shift
SUBS=()
while [ "$1" != "--" ]; do SUBS+=("$1"); shift; done
shift  # drop --
ALT=$(IFS='|'; echo "${SUBS[*]}")   # INQ|PUT|READ|SET

for f in "$@"; do
  # 1. CLASS.SUBMOD::proc -> .::proc
  sed -i -E "s/\b${CLASS}\.(${ALT})::([a-zA-Z])/.::\2/g" "$f"
  # 2. CLASS.SUBMOD:proc  -> .proc
  sed -i -E "s/\b${CLASS}\.(${ALT}):([a-zA-Z])/.\2/g" "$f"
  # 3. .SUBMOD::proc -> .::proc   (remaining self-submodule)
  sed -i -E "s/\.(${ALT})::([a-zA-Z])/.::\2/g" "$f"
  # 4. .SUBMOD:proc  -> .proc
  sed -i -E "s/\.(${ALT}):([a-zA-Z])/.\2/g" "$f"
  # 5. .:proc -> .proc   (same-submodule generic; single colon only, NOT .::proc)
  sed -i -E "s/\.:([a-zA-Z])/.\1/g" "$f"
done
echo "converted: $*"
