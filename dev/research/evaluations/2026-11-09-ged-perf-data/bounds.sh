#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR=$HOME/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
mkdir -p build/tmp build/scratch
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" dev/research/evaluations/2026-11-09-ged-perf-data/check_inherited_bounds.cpp -o build/scratch/ged-perf-inherited-bounds
python3 dev/research/evaluations/2026-11-07-ged-free-data/check_bounds.py build/scratch/ged-perf-inherited-bounds
