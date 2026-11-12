#!/usr/bin/env bash
set -euo pipefail
repo=$(cd "$(dirname "$0")/../../../.." && pwd)
cd "$repo"
mkdir -p build/scratch build/tmp
export TMPDIR="$repo/build/tmp"
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -Ibindings -IunivariateML-cpp/include -I"$boost" \
  dev/research/evaluations/2026-11-12-api-stops-data/replay.cpp \
  -o build/scratch/api-stops-replay
build/scratch/api-stops-replay
