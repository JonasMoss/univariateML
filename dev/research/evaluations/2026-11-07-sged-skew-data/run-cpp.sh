#!/bin/bash
set -euo pipefail
python3 dev/research/evaluations/2026-11-07-sged-skew-data/check.py
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$boost" dev/research/evaluations/2026-11-07-sged-skew-data/benchmark.cpp -o build/scratch/benchmark
build/scratch/benchmark
just cpp-test
UMVL_TEST_TIER=full just cpp-test
just cpp-test-no-long-double
