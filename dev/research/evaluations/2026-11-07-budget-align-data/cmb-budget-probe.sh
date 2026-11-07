#!/bin/bash
set -eu
base=dev/research/evaluations/2026-11-07-budget-align-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -Ibindings -IunivariateML-cpp/tests -IunivariateML-cpp/include -I"$boost" "$base/cmb-budget-probe.cpp" -o build/scratch/cmb-budget-probe
build/scratch/cmb-budget-probe
