#!/usr/bin/env bash
# From root, after the baseline include snapshot exists, under one CPU slot.
set -euo pipefail
source build/scratch/env.sh
data=dev/research/evaluations/2026-11-06-slow-held-data
python3 "$data/census.py"
c++ -O2 -std=c++17 -Ibuild/scratch/before-include -Ibindings -IunivariateML-cpp/tests -I"$(Rscript -e 'cat(system.file("include", package="BH"))')" build/scratch/census.cpp -o build/scratch/census
build/scratch/census > "$data/census-baseline.tsv"
