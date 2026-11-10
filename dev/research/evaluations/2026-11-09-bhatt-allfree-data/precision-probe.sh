#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-09-bhatt-allfree-data/precision-probe.cpp -o build/scratch/bhatt-precision-probe
build/scratch/bhatt-precision-probe
