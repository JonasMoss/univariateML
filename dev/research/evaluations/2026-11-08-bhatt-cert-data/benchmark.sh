#!/usr/bin/env bash
set -euo pipefail
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
flags=()
if [[ ${2:-} == baseline ]]; then flags=(-DBHATT_BASELINE); fi
"${CXX:-g++}" "${flags[@]}" -O2 -std=c++17 -Wall -Wextra -IunivariateML-cpp/include -I"$boost" \
  "${1:-dev/research/evaluations/2026-11-08-bhatt-cert-data/timings.cpp}" -o build/scratch/bhatt-timing
build/scratch/bhatt-timing
