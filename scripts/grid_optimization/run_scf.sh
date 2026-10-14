#!/bin/bash
# In the run folder (the current directory): run the SCF of every system in
# joblist_<basis>.txt, 8 at a time, skipping any whose stdout says the SCF
# converged. Safe to stop and start again.
S=$(cd "$(dirname "$0")" && pwd); T=$(cd "$S/../.." && pwd)
B=${1:-def2-SVP}
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 T
run() {
  cd "$1" || exit
  grep -q "SCF has converged" stdout 2>/dev/null && { echo "$1 already converged"; exit; }
  /usr/bin/time -p timeout 7200 ../../tonto.grid --basis-library "$T/basis_sets" > run.log 2>&1
  echo "$1 rc=$? $(grep '^real' run.log) $(date +%H:%M:%S)"
}
export -f run
xargs -P 8 -I{} bash -c 'run {}' < joblist_$B.txt
echo "== ALL DONE $(date)"
