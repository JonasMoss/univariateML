#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p build/tmp build/scratch
c++ -O2 -std=c++17 -IunivariateML-cpp/include dev/research/evaluations/2026-11-07-evt-joint-data/shape-probe.cpp -o build/scratch/shape-probe
build/scratch/shape-probe
export CCACHE_DIR="$HOME/.cache/ccache/univariateML" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
cmake -S univariateML-cpp -B univariateML-cpp/build -DCMAKE_CXX_COMPILER_LAUNCHER=ccache -DBoost_INCLUDE_DIR="$(Rscript -e 'cat(system.file("include",package="BH"))')"
cmake --build univariateML-cpp/build --target distribution_tests -j4
univariateML-cpp/build/tests/distribution_tests '--test-case=gev:*,frechet:*'
