#!/usr/bin/env bash
set -euo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
sha256sum univariateML-cpp/include/cppunivariate/distributions/{gev,frechet}/mle.hpp univariateML-cpp/include/cppunivariate/distributions/frechet/certificate.hpp
just cpp-test
UMVL_TEST_TIER=full just cpp-test
univariateML-cpp/build/tests/distribution_tests '--test-case=gev:*,frechet:*'
# Recheck the affected no-long-double families after the final hold-stop fix.
cmake --build build/cpp-no-long-double --target distribution_tests -j4
build/cpp-no-long-double/tests/distribution_tests '--test-case=gev:*,frechet:*'
# The R harness uses four workers. The local targeted single-slot job must
# finish before this four-worker phase is started by the caller.
just test
just test-full

just js-build
just js-test
just site-check
