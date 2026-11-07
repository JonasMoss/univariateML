#!/bin/bash
set -eu
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -Ibindings -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-07-budget-align-data/budget-probe-held.cpp -o build/scratch/budget-probe-held
build/scratch/budget-probe-held
