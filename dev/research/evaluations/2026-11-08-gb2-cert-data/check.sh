#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
bench="build/scratch/gb2-check-$$"
trap 'rm -f "$bench"' EXIT
c++ -std=c++17 -O2 -I"${GB2_INCLUDE:-univariateML-cpp/include}" -I"$bh" dev/research/evaluations/2026-11-08-gb2-cert-data/check.cpp -o "$bench"
"$bench"
