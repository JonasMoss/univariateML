#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch
export CCACHE_DIR="$HOME/.cache/ccache/univariateML" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
cmake -S univariateML-cpp -B univariateML-cpp/build -DCMAKE_CXX_COMPILER_LAUNCHER=ccache -DBoost_INCLUDE_DIR="$(Rscript -e 'cat(system.file("include",package="BH"))')"
cmake --build univariateML-cpp/build --target distribution_tests -j "${CMAKE_BUILD_PARALLEL_LEVEL:-1}"
univariateML-cpp/build/tests/distribution_tests '--test-case=gev:*,frechet:*'
