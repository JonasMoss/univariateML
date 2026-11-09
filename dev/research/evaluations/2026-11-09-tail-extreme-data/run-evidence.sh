#!/bin/sh
# Run from the worktree root, inside the workstation CPU-slot wrapper.
set -eu
evidence=dev/research/evaluations/2026-11-09-tail-extreme-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
for mode in native double; do
  flags=
  if [ "$mode" = double ]; then flags=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS; fi
  for probe in probe edge std-overflow; do
    c++ -std=c++17 -O1 $flags -IunivariateML-cpp/include -I"$boost" "$evidence/$probe.cpp" -o "build/scratch/$probe-$mode"
    "build/scratch/$probe-$mode" > "$evidence/$mode-$probe.csv"
  done
  python3 "$evidence/compare.py" "$evidence/$mode-probe.csv" > "$evidence/$mode-comparison.json"
  python3 "$evidence/compare-edge.py" "$evidence/$mode-edge.csv" > "$evidence/$mode-edge-comparison.json"
  python3 "$evidence/compare-std-overflow.py" "$evidence/$mode-std-overflow.csv" > "$evidence/$mode-std-overflow-comparison.json"
done
