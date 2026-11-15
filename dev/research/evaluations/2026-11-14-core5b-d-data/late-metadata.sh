#!/usr/bin/env bash
set -euo pipefail
ulimit -c 0
export TMPDIR=$PWD/build/tmp
D=dev/research/evaluations/2026-11-14-core5b-d-data
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
for mode in native double; do
  args=();if [ "$mode" = double ]; then args+=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS);fi
  c++ -std=c++17 -O2 "${args[@]}" -I. -IunivariateML-cpp/include -I"$bh" "$D/late-metadata.cpp" -o "build/scratch/late-metadata-$mode"
  "build/scratch/late-metadata-$mode"
done
