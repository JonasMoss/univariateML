#!/bin/sh
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
for suite in cpp-test test cpp-test-no-long-double; do
  echo "BEGIN $suite $(date -u +%FT%TZ)"
  just "$suite" > "build/scratch/t-close-$suite.log" 2>&1 || { tail -n 100 "build/scratch/t-close-$suite.log"; exit 1; }
  tail -n 12 "build/scratch/t-close-$suite.log"
  echo "END $suite $(date -u +%FT%TZ)"
done
echo "BEGIN cpp-full $(date -u +%FT%TZ)"
UMVL_TEST_TIER=full just cpp-test > build/scratch/t-close-cpp-full.log 2>&1 || { tail -n 100 build/scratch/t-close-cpp-full.log; exit 1; }
tail -n 12 build/scratch/t-close-cpp-full.log
echo "END cpp-full $(date -u +%FT%TZ)"
echo "BEGIN test-full $(date -u +%FT%TZ)"
just test-full > build/scratch/t-close-test-full.log 2>&1 || { tail -n 120 build/scratch/t-close-test-full.log; exit 1; }
tail -n 12 build/scratch/t-close-test-full.log
echo "END test-full $(date -u +%FT%TZ)"
sha256sum univariateML-cpp/include/cppunivariate/distributions/lst/certificate.hpp \
 univariateML-cpp/include/cppunivariate/distributions/lst/test.cpp \
 univariateML-cpp/include/cppunivariate/distributions/std/test.cpp \
 bindings/r/distributions/std/test.R
sha256sum build/scratch/t-close-{cpp-test,test,cpp-test-no-long-double,cpp-full,test-full}.log 2>/dev/null || \
 sha256sum build/scratch/t-close-cpp-test.log build/scratch/t-close-test.log build/scratch/t-close-cpp-test-no-long-double.log build/scratch/t-close-cpp-full.log build/scratch/t-close-test-full.log
