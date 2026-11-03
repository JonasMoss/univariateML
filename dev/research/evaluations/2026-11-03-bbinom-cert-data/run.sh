#!/bin/sh
# From the worktree root: /tmp/umvl-cpu/run sh .../run.sh
# This script does no waiting or nested CPU-slot acquisition.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
study=dev/research/evaluations/2026-11-03-bbinom-cert-data
python3 "$study/check.py"
boost_headers=$(Rscript -e 'cat(system.file("include", package="BH"))')
ccache g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_headers" \
  dev/research/evaluations/2026-11-02-bbinom-map-data/check.cpp \
  -o build/scratch/bbinom-cert-map-check
python3 "$study/verify.py"
