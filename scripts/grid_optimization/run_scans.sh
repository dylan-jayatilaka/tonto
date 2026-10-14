#!/bin/bash
# In the run folder (the current directory): run the scans in scanlist.txt,
# 8 at a time, skipping finished outputs, then add them to the store.
# Safe to stop and start again: run collect.py, scan.py and this again.
S=$(cd "$(dirname "$0")" && pwd); T=$(cd "$S/../.." && pwd)
B=${1:-def2-SVP}
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 T
run() {
  d=${1% *}; s=${1#* }
  cd "$d" || exit
  # Finished, and newer than its input? Then nothing to do
  [ scan_$s.out -nt scan_$s.in ] && grep -q "CPU time taken" scan_$s.out && { echo "$d $s already done"; exit; }
  /usr/bin/time -p timeout 14400 ../../tonto.grid --input scan_$s.in --output scan_$s.out \
     --basis-library "$T/basis_sets" > scan_$s.log 2>&1
  echo "$d $s rc=$? $(grep '^real' scan_$s.log) $(date +%H:%M:%S)"
}
export -f run
xargs -P 8 -I{} bash -c 'run "{}"' < scanlist.txt
python3 "$S/collect.py" $B
echo "== ALL DONE $(date)"
