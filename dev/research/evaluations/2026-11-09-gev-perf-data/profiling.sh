#!/bin/bash
# Invoke through /tmp/umvl-cpu/run, from the worktree root.
set -euo pipefail
export TMPDIR="$PWD/build/tmp"
out=dev/research/evaluations/2026-11-09-gev-perf-data
for stage in baseline final;do
  path=$(python3 "$out/profile.py" "$stage" instrument)
  c++ -std=c++17 -O2 -I"$path" -IunivariateML-cpp/include -Ibuild/scratch -include "$path/instrument.hpp" "$path/benchmark.cpp" -o "$path/benchmark"
  "$path/benchmark" > "$out/$stage-profile.jsonl" 2> "$out/$stage-profile.txt"
done
