#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR=$HOME/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
mkdir -p build/tmp build/scratch
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
files=dev/research/evaluations/2026-11-09-ged-perf-data
old=dev/research/evaluations/2026-11-07-ged-free-data
bash "$files/target.sh"
bash "$old/o1-oracles.sh"
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" "$files/check_inherited.cpp" -o build/scratch/ged-perf-inherited
python3 "$old/check_scores.py" build/scratch/ged-perf-inherited
