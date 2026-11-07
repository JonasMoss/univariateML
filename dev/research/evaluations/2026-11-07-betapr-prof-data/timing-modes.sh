#!/bin/sh
# Invoke from repository root through /tmp/umvl-cpu/run (one CPU slot).
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
record=dev/research/evaluations/2026-11-07-betapr-prof-data
scratch=build/scratch/betapr-prof
mkdir -p "$scratch/baseline-include"
cp -r univariateML-cpp/include/cppunivariate "$scratch/baseline-include/"
git show 7c52f905f21bee7d997d59211d4093e2593ec727:univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp > "$scratch/baseline-include/cppunivariate/distributions/betapr/mle.hpp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -I"$scratch/baseline-include" -I"$bh" "$record/check.cpp" -o "$scratch/baseline"
"$scratch/baseline" > "$record/before.txt"
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$bh" "$record/check.cpp" -o "$scratch/current"
"$scratch/current" > "$record/after.txt"
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$bh" "$record/modes.cpp" -o "$scratch/modes"
"$scratch/modes" > "$record/modes.txt"
