#!/usr/bin/env bash
set -euo pipefail
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
printf 'SOURCE FINGERPRINTS\n'
sha256sum univariateML-cpp/include/cppunivariate/distributions/{tnorm,cmb,cmp}/{mle.hpp,test.cpp,README.md} univariateML-cpp/include/cppunivariate/distributions/cmb/acceptance.hpp bindings/common/distributions/{tnorm,cmb,cmp}/fit.hpp bindings/r/distributions/{tnorm,cmb,cmp}/test.R bindings/js/test.mjs
printf 'FINAL C++ DEV\n'
UMVL_TEST_TIER=dev just cpp-test
printf 'FINAL C++ FULL\n'
UMVL_TEST_TIER=full just cpp-test
printf 'FINAL C++ NO LONG DOUBLE\n'
just cpp-test-no-long-double
printf 'FINAL R DEV\n'
just test
printf 'FINAL R FULL\n'
just test-full
printf 'ALL FINAL NATIVE/R SUITES PASSED\n'
