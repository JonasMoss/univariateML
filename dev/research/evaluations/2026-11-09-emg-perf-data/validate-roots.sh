#!/usr/bin/env bash
# Run through the one-slot wrapper. Existing independent oracle copied here
# so outputs never overwrite the previous certification job's evidence.
set -euo pipefail
p=dev/research/evaluations/2026-11-09-emg-perf-data
for mode in native double; do
  flags=(); suffix=""
  if [ "$mode" = double ]; then flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); suffix=-double; fi
  c++ -std=c++17 -O2 "${flags[@]}" -IunivariateML-cpp/include -IunivariateML-cpp/tests "$p/outer-oracle.cpp" -o "build/scratch/emg-perf/outer$suffix"
  "build/scratch/emg-perf/outer$suffix" > "$p/outer$suffix.csv"
  if [ "$mode" = double ]; then python3 "$p/outer-oracle.py" --double; else python3 "$p/outer-oracle.py"; fi
done
