#!/usr/bin/env bash
# Run from the worktree root through /tmp/umvl-cpu/run.
set -euo pipefail
data=dev/research/evaluations/2026-11-26-huber-flat-set-data
scratch=build/scratch/huber-replay
mkdir -p "$scratch"
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
for revision in 19b2fb7b 2d89fd4a; do
  mkdir -p "$scratch/$revision"
  git archive "$revision" univariateML-cpp/include | tar -x -C "$scratch/$revision"
  c++ -std=c++17 -O2 -I"$boost" -I"$scratch/$revision/univariateML-cpp/include" \
    "$data/probe.cpp" -o "$scratch/probe"
  "$scratch/probe" > "$data/$revision.txt"
done
c++ -std=c++17 -O2 -DHUBER_EXACT_PROBE -I"$boost" -IunivariateML-cpp/include \
  "$data/probe.cpp" -o "$scratch/probe"
"$scratch/probe" > "$data/fixed.txt"
python3 "$data/check-gap.py" "$scratch/probe" > "$data/exact-gap.txt"
python3 "$data/reference.py" > "$data/reference.txt"
