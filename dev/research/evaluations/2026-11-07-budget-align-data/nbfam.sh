#!/bin/bash
set -eu
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
base=dev/research/evaluations/2026-11-07-budget-align-data
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$base/nbfam-probe.cpp" -o build/scratch/nbfam-probe
python3 "$base/nbfam-check.py" build/scratch/nbfam-probe build/scratch/nbfam-current.json >/dev/null
cat build/scratch/nbfam-current.json
