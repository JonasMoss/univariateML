#!/bin/sh
# Invoke through run (one slot). This is a diagnostic, not an all-free fit.
set -eu
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/bnbinom-joint
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$bh" dev/research/evaluations/2026-11-08-bnbinom-joint-data/profile-probe.cpp -o build/scratch/bnbinom-joint/profile-probe
build/scratch/bnbinom-joint/profile-probe build/scratch/bnbinom-joint/counts.txt > dev/research/evaluations/2026-11-08-bnbinom-joint-data/profile-probe.json
python3 -m json.tool dev/research/evaluations/2026-11-08-bnbinom-joint-data/profile-probe.json
