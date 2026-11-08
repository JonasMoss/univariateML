#!/usr/bin/env bash
set -euo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=3 CTEST_PARALLEL_LEVEL=3
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -O2 -std=c++17 -I"$boost" -IunivariateML-cpp/include dev/research/evaluations/2026-11-07-bbinom-face-data/native-check.cpp -o build/scratch/moment-native
build/scratch/moment-native > build/scratch/moment-native.txt
UMVL_TEST_TIER=full just cpp-test > build/scratch/moment-cpp-full.log 2>&1
cmake --build build/cpp-no-long-double --target distribution_tests -j3 > build/scratch/moment-no-long-double-build.log 2>&1
ctest --test-dir build/cpp-no-long-double -R '^distribution_bbinom$' --output-on-failure > build/scratch/moment-no-long-double-bbinom.log 2>&1
just test-full > build/scratch/moment-r-full.log 2>&1
