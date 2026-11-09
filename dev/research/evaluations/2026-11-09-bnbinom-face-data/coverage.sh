#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/bnbinom-face/coverage
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic -IunivariateML-cpp/include -I"$bh" \
  dev/research/evaluations/2026-11-09-bnbinom-face-data/coverage.cpp -o build/scratch/bnbinom-face/coverage/check
build/scratch/bnbinom-face/coverage/check "$1"
