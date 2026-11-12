#!/usr/bin/env bash
set -euo pipefail
repo=$(cd "$(dirname "$0")/../../../.." && pwd)
cd "$repo"
mkdir -p build/scratch/api-stops-baseline build/tmp
export TMPDIR="$repo/build/tmp"
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
git archive 0019c6b3 univariateML-cpp/include | tar -x -C build/scratch/api-stops-baseline
for version in baseline current; do
  if [[ $version == baseline ]]; then headers=build/scratch/api-stops-baseline/univariateML-cpp/include; else headers=univariateML-cpp/include; fi
  c++ -std=c++17 -O0 -I"$headers" -I"$boost" dev/research/evaluations/2026-11-12-api-stops-data/transports.cpp -o "build/scratch/api-stops-transports-$version"
  printf '%s\n' "$version"
  "build/scratch/api-stops-transports-$version"
done
