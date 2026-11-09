#!/bin/bash
set -eu
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
python3 dev/research/evaluations/2026-11-09-golden-ref-port-data/capture.py > build/scratch/capture.cpp
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -std=c++17 -O2 -DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS -I. -IunivariateML-cpp/include -I"$boost" build/scratch/capture.cpp -o build/scratch/capture-double
build/scratch/capture-double
