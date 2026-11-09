#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
evidence=dev/research/evaluations/2026-11-09-bnbinom-face-data
c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic -IunivariateML-cpp/include -I"$bh" "$evidence/probe.cpp" -o build/scratch/bnbinom-face/probe-final
build/scratch/bnbinom-face/probe-final > "$evidence/probe.json"
