#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CMAKE_BUILD_PARALLEL_LEVEL=3 CTEST_PARALLEL_LEVEL=3
mkdir -p build/tmp build/scratch
boost_include=$(Rscript -e 'cat(system.file("include",package="BH"))')
cmake -S univariateML-cpp -B univariateML-cpp/build -DCMAKE_CXX_COMPILER_LAUNCHER=ccache -DBoost_INCLUDE_DIR="$boost_include"
cmake --build univariateML-cpp/build --target distribution_tests -j3
ctest --test-dir univariateML-cpp/build --output-on-failure -R '^distribution_sged$'
