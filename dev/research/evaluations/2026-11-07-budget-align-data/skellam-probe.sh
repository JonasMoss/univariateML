#!/bin/bash
set -eu
base=dev/research/evaluations/2026-11-07-budget-align-data
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -Ibindings -IunivariateML-cpp/include -I"$boost" "$base/skellam-probe.cpp" -o build/scratch/skellam-probe
build/scratch/skellam-probe
