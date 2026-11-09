#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$bh" dev/research/evaluations/2026-11-08-bnbinom-allfree-data/root-probe.cpp -o build/scratch/bnbinom-allfree/root-probe
build/scratch/bnbinom-allfree/root-probe > dev/research/evaluations/2026-11-08-bnbinom-allfree-data/root-probe.json
