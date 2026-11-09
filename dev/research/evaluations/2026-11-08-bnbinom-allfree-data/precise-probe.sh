#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$bh" dev/research/evaluations/2026-11-08-bnbinom-allfree-data/cover-probe.cpp -o build/scratch/bnbinom-allfree/precise-probe
build/scratch/bnbinom-allfree/precise-probe build/scratch/bnbinom-allfree/counts.txt 600 normal precondition 2.5e-9 > dev/research/evaluations/2026-11-08-bnbinom-allfree-data/precise-probe.json
