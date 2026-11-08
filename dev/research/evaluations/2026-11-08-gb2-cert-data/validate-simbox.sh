#!/bin/bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
mkdir -p build/tmp build/scratch
record=build/scratch/gb2-validation
mkdir -p "$record"
run_check() {
 local name=$1
 shift
 if "$@" > "$record/$name.log" 2>&1; then
  printf '%s PASS\n' "$name"
  tail -8 "$record/$name.log"
 else
  rc=$?
  tail -90 "$record/$name.log"
  exit "$rc"
 fi
}
source_manifest() {
 sha256sum univariateML-cpp/include/cppunivariate/distributions/{gb2,fpareto}/{mle.hpp,README.md,test.cpp} \
  univariateML-cpp/include/cppunivariate/distributions/gb2/power_certificate.hpp \
  bindings/common/distributions/{gb2,fpareto}/fit.hpp \
  bindings/r/distributions/{gb2,fpareto}/test.R bindings/js/fit-test.mjs
}
source_manifest > "$record/source-before.sha256"
run_check cpp-dev just cpp-test
run_check cpp-full env UMVL_TEST_TIER=full just cpp-test
run_check cpp-no-long-double just cpp-test-no-long-double
run_check r-dev just test
run_check r-full just test-full
source_manifest > "$record/source-after.sha256"
cmp "$record/source-before.sha256" "$record/source-after.sha256"
