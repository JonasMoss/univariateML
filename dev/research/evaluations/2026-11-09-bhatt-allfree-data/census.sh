#!/usr/bin/env bash
set -euo pipefail
mkdir -p build/tmp build/scratch
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" dev/research/evaluations/2026-11-09-bhatt-allfree-data/census.cpp -o build/scratch/bhatt-census
build/scratch/bhatt-census "${1:-0}"
