#!/usr/bin/env bash
# Run this sequential computation through /tmp/umvl-cpu/run.
set -euo pipefail
export TMPDIR=$PWD/build/tmp
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
d=dev/research/evaluations/2026-11-10-gev-held2-data
python3 "$d/make-phase-check.py"
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$b" "$d/phase-check.cpp" -o build/scratch/phase-check
build/scratch/phase-check 100 > "$d/phase-final-100.csv"
build/scratch/phase-check 1000 > "$d/phase-final-1000.csv"
