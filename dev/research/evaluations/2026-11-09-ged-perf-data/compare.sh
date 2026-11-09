#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR=$HOME/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
mkdir -p build/tmp build/scratch
files=dev/research/evaluations/2026-11-09-ged-perf-data
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
for variant in before after; do
 ccache c++ -std=c++17 -O2 -I"build/scratch/ged-perf/$variant/include" -I"$boost_dir" "$files/compare.cpp" -o "build/scratch/ged-perf/$variant-compare.exe"
 "build/scratch/ged-perf/$variant-compare.exe" > "$files/$variant-compare.txt"
done
python3 "$files/compare.py"
