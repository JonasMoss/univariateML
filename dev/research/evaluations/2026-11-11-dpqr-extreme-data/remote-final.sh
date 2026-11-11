#!/bin/bash
# Called by simbox-run; the outer wrapper supplies nice and single BLAS threads.
set -u
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
mkdir -p build/scratch/remote-final
rc=0
TRUTH_EXTREME_DIGITS=160 TRUTH_EXTREME_TIMEOUT=120 python3 "$h/generate.py" bnbinom huber sn dgamma > build/scratch/remote-final/references.log 2>&1 || rc=$?
printf 'reference-corrections\tsimbox\t%s\n' "$rc" > "$h/remote-final-runs.tsv"
if [ "$rc" -gt 1 ]; then exit "$rc"; fi
python3 "$h/finalize-metadata.py" || exit $?
failed=0
for recipe in cpp-test-no-long-double test test-full; do
  just "$recipe" > "build/scratch/remote-final/$recipe.log" 2>&1
  code=$?
  printf '%s\tsimbox\t%s\n' "$recipe" "$code" | tee -a "$h/remote-final-runs.tsv"
  tail -20 "build/scratch/remote-final/$recipe.log"
  if [ "$code" -ne 0 ]; then failed=1; fi
done
exit "$failed"
