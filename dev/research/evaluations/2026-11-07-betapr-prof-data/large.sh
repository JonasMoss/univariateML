#!/bin/sh
# Run after timing-modes.sh, under one CPU slot from the repository root.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
record=dev/research/evaluations/2026-11-07-betapr-prof-data
scratch=build/scratch/betapr-prof
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -I"$scratch/baseline-include" -I"$bh" "$record/large.cpp" -o "$scratch/large-before"
"$scratch/large-before" > "$record/large-before.txt"
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$bh" "$record/large.cpp" -o "$scratch/large-after"
"$scratch/large-after" > "$record/large-after.txt"
