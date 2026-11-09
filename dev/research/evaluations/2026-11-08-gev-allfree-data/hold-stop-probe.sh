#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
c++ -O2 -std=c++17 -I. -IunivariateML-cpp/include dev/research/evaluations/2026-11-08-gev-allfree-data/hold-stop-probe.cpp -o build/scratch/hold-stop-probe
build/scratch/hold-stop-probe
