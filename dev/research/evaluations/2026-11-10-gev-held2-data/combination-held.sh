#!/usr/bin/env bash
# Run from the worktree root, already inside one CPU slot (no nested locks).
set -euo pipefail
export TMPDIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
d=dev/research/evaluations/2026-11-10-gev-held2-data
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$b" "$d/combination-study.cpp" -o build/scratch/combination-policy-study
for n in 100 1000; do
  build/scratch/combination-policy-study "$n" "$d/combination-policy-roots-$n.jsonl" > "$d/combination-policy-$n.csv"
done
python3 "$d/combination-reference.py" "$d/combination-policy-roots-100.jsonl" "$d/combination-policy-roots-1000.jsonl" > "$d/combination-policy-reference.jsonl"
python3 "$d/combination-summarize.py"
