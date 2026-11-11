#!/usr/bin/env bash
# Run through the CPU-slot wrapper from the repository root. The baseline
# extraction stays in build/, never in the retained evaluation data.
set -eu
study=dev/research/evaluations/2026-11-11-cens-easy-data
mkdir -p build/scratch/bnbinom-baseline
if [[ ${1:-} == committed ]]; then
  git archive 206e979f univariateML-cpp/include | tar -xf - -C build/scratch/bnbinom-baseline
  include=build/scratch/bnbinom-baseline/univariateML-cpp/include
  git show 206e979f:univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json > build/scratch/bnbinom-baseline/golden.json
else
  # simbox-run excludes .git. The local committed comparison verifies that
  # this header closure is unchanged; no censoring headers are included here.
  include=univariateML-cpp/include
  cp univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json build/scratch/bnbinom-baseline/golden.json
fi
python3 -c 'import json; print(" ".join(map(str,map(int,json.load(open("build/scratch/bnbinom-baseline/golden.json"))["data"]))))' > build/scratch/bnbinom-baseline/sample.txt
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -std=c++17 -O2 -DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS -I"$include" -I"$boost" "$study/bnbinom-baseline.cpp" -o build/scratch/bnbinom-baseline/check
build/scratch/bnbinom-baseline/check < build/scratch/bnbinom-baseline/sample.txt
