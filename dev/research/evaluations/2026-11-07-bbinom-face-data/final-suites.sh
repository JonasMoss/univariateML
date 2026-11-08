#!/usr/bin/env bash
set -euo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=3 CTEST_PARALLEL_LEVEL=3
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -O2 -std=c++17 -I"$boost" -IunivariateML-cpp/include dev/research/evaluations/2026-11-07-bbinom-face-data/native-check.cpp -o build/scratch/closure-native
build/scratch/closure-native > build/scratch/closure-native.txt
just cpp-test > build/scratch/closure-cpp-dev.log 2>&1
UMVL_TEST_TIER=full just cpp-test > build/scratch/closure-cpp-full.log 2>&1
just cpp-test-no-long-double > build/scratch/closure-no-long-double.log 2>&1
just test > build/scratch/closure-r-dev.log 2>&1
just test-full > build/scratch/closure-r-full.log 2>&1
