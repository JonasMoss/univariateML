#!/bin/bash
# Standalone replay avoids regenerating the package alongside final-r.sh.
set -eu
base=dev/research/evaluations/2026-11-07-budget-align-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$base/cmb-r-seed-target.cpp" -o build/scratch/cmb-r-seed-target
build/scratch/cmb-r-seed-target
