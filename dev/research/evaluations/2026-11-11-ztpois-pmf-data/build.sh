#!/usr/bin/env bash
set -euo pipefail
here=dev/research/evaluations/2026-11-11-ztpois-pmf-data
base=build/scratch/ztpois-baseline
mkdir -p "$base"
# Rebuild baseline from the recorded commit; no archived sources in evidence.
git archive 3b2a16fe univariateML-cpp/include bindings/common | tar -x -C build/scratch/ztpois-baseline
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
for mode in native double; do
 flags=(); if [[ $mode == double ]]; then flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); fi
 c++ -std=c++17 -O2 "${flags[@]}" -IunivariateML-cpp/include -I"$boost" "$here/kernel.cpp" -o "build/scratch/kernel-$mode"
 for phase in after before; do
  root=.; inc=univariateML-cpp/include
  if [[ $phase == before ]]; then root=build/scratch/ztpois-baseline; inc=build/scratch/ztpois-baseline/univariateML-cpp/include; fi
  c++ -std=c++17 -O2 "${flags[@]}" -I"$root" -I"$inc" -I. -IunivariateML-cpp/include -I"$boost" "$here/replay.cpp" -o "build/scratch/replay-$phase-$mode"
 done
done
