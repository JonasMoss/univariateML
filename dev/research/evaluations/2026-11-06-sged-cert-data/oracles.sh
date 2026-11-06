#!/usr/bin/env bash
# Invoke from repository root through /tmp/umvl-cpu/run.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
data=dev/research/evaluations/2026-11-06-sged-cert-data
for suffix in '' '-scale'; do
  c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" "$data/oracle$suffix.cpp" -o "build/scratch/sged-oracle$suffix"
  "build/scratch/sged-oracle$suffix" > "$data/oracle$suffix.txt"
  python3 "$data/check$suffix.py"
done
