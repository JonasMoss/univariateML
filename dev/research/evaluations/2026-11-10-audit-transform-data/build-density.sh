#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O1 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-10-audit-transform-data/density-check.cpp -o build/scratch/density-check
