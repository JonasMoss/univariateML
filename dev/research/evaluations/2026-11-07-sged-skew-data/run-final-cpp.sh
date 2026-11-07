#!/bin/bash
set -euo pipefail
c++ --version | head -1
python3 dev/research/evaluations/2026-11-07-sged-skew-data/check.py
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-07-sged-skew-data/benchmark.cpp -o build/scratch/benchmark
build/scratch/benchmark
just cpp-test
UMVL_TEST_TIER=full just cpp-test
cmake --build build/cpp-no-long-double -j6
ctest --test-dir build/cpp-no-long-double --output-on-failure -R '^distribution_sged$'
