#!/bin/bash
set -euo pipefail
job_data=dev/research/evaluations/2026-11-06-stops-cont-data
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
for tree in baseline current; do
  if [ "$tree" = baseline ]; then include="${ST_BASELINE_INCLUDE:-build/scratch/st-baseline-include}"; else include=univariateML-cpp/include; fi
  g++ -std=c++17 -O2 -g -fpic -fopenmp -I"$include" -I"$boost" "$job_data/check-st-equivariance.cpp" -o "build/scratch/st-remote-$tree"
  echo "$tree"
  "build/scratch/st-remote-$tree" < "$job_data/st-sample.txt"
done
