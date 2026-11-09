#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-08-gev-allfree-data/joint-seeded.cpp -o build/scratch/joint-seeded
build/scratch/joint-seeded
