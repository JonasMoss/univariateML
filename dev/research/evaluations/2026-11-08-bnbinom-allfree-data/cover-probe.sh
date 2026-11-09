#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp"
evidence=dev/research/evaluations/2026-11-08-bnbinom-allfree-data
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$bh" "$evidence/cover-probe.cpp" -o build/scratch/bnbinom-allfree/cover-probe
build/scratch/bnbinom-allfree/cover-probe build/scratch/bnbinom-allfree/counts.txt > "$evidence/cover-probe.json"
build/scratch/bnbinom-allfree/cover-probe build/scratch/bnbinom-allfree/counts.txt 600 diagonal > "$evidence/diagonal-probe.json"
build/scratch/bnbinom-allfree/cover-probe build/scratch/bnbinom-allfree/counts.txt 6000 > "$evidence/cover-probe-large.json"
