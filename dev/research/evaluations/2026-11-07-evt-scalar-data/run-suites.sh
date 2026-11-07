#!/bin/bash
set -euo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
just cpp-test
UMVL_TEST_TIER=full just cpp-test
just cpp-test-no-long-double
just test
just test-full
c++ -O2 -std=c++17 dev/research/evaluations/2026-11-07-evt-scalar-data/search.cpp -o build/scratch/evt-search
build/scratch/evt-search
python3 dev/research/evaluations/2026-11-07-evt-scalar-data/check_math.py
c++ -O2 -std=c++17 -IunivariateML-cpp/include dev/research/evaluations/2026-11-07-evt-scalar-data/check_edges.cpp -o build/scratch/evt-edges
build/scratch/evt-edges
