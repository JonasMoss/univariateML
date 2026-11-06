#!/usr/bin/env bash
# Run from root under /tmp/umvl-cpu/run after before/after binaries exist.
set -euo pipefail
source build/scratch/env.sh
data=dev/research/evaluations/2026-11-06-slow-held-data
: > "$data/cpu-timings.txt"
: > "$data/cpu-fit-outputs.txt"
for rep in 1 2 3; do
 for id in tlambda emg bhatt; do
  masks='0 1 2 3 4 5 6 7'
  for mask in $masks; do
   for binary in before after; do
    /usr/bin/time -f "$id $mask $binary $rep %U %S" -a -o "$data/cpu-timings.txt" build/scratch/$binary "$id" 0 "$mask" >> "$data/cpu-fit-outputs.txt"
   done
  done
 done
done
