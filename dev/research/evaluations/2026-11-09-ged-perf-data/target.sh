#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR=$HOME/.cache/ccache/univariateML CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
mkdir -p build/tmp build/scratch
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost_dir" -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" univariateML-cpp/tests/distribution_tests_main.cpp univariateML-cpp/include/cppunivariate/distributions/ged/test.cpp -o build/scratch/ged-perf-target
build/scratch/ged-perf-target
