#!/bin/sh
# Invoke through run: sequential one-thread checks; site inherits one CPU.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
evidence=dev/research/evaluations/2026-11-08-bnbinom-allfree-data
scratch=build/scratch/bnbinom-allfree
sh "$evidence/targeted-final.sh" > "$scratch/targeted-both-final.log" 2>&1
sh "$evidence/final-replay.sh" > "$scratch/final-replay.log" 2>&1
for stage in js-build js-test; do
  {
    echo "START workstation-$stage $(date -u +%FT%TZ)"
    just "$stage"
    echo "GREEN workstation-$stage $(date -u +%FT%TZ)"
  } > "$scratch/$stage-final.log" 2>&1
done
cpu=$(python3 -c 'import os; print(min(os.sched_getaffinity(0)))')
taskset -c "$cpu" sh "$evidence/site-final.sh" > "$scratch/site-final-current.log" 2>&1
python3 "$evidence/audit.py"
