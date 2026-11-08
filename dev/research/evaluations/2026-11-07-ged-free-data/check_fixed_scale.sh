#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost_dir" dev/research/evaluations/2026-11-07-ged-free-data/check_fixed_scale.cpp -o build/scratch/check_fixed_scale
build/scratch/check_fixed_scale
