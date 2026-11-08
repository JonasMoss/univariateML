#!/bin/bash
set -euo pipefail
mkdir -p build/tmp build/scratch
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-07-binom-faces-data/timing.cpp -o build/scratch/binom-faces-timing
build/scratch/binom-faces-timing
