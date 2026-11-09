#!/bin/bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp"
out=dev/research/evaluations/2026-11-09-gev-perf-data
for stage in baseline final;do
  path=build/scratch/perf-$stage
  c++ -std=c++17 -O2 -I"$path" -IunivariateML-cpp/include -Ibuild/scratch "$out/controls.cpp" -o "$path/controls"
  "$path/controls" > "$out/$stage-controls.txt"
done
diff -u "$out/baseline-controls.txt" "$out/final-controls.txt"
