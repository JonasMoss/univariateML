#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -I"${GB2_INCLUDE:-univariateML-cpp/include}" -I"$bh" dev/research/evaluations/2026-11-08-gb2-power-data/check.cpp -o build/scratch/gb2-power-check
build/scratch/gb2-power-check
