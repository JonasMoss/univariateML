#!/bin/bash
# Invoke through /tmp/umvl-cpu/run; builds and ctest use one worker.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
run_check() {
 printf '\nBEGIN %s\n' "$*"
 /usr/bin/time -p "$@"
 printf 'GREEN %s\n' "$*"
}
sha256sum univariateML-cpp/include/cppunivariate/distributions/ged/{mle.hpp,shape_certificate.hpp,free_shape_bounds.hpp,test.cpp,README.md,golden.json}
c++ --version | head -1
run_check just cpp-test
run_check env UMVL_TEST_TIER=full just cpp-test
run_check just cpp-test-no-long-double
sha256sum univariateML-cpp/include/cppunivariate/distributions/ged/{mle.hpp,shape_certificate.hpp,free_shape_bounds.hpp,test.cpp,README.md,golden.json}
