#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CMAKE_BUILD_PARALLEL_LEVEL=3 CTEST_PARALLEL_LEVEL=3
mkdir -p build/tmp build/scratch
c++ --version | head -1
sha256sum univariateML-cpp/include/cppunivariate/distributions/ged/{mle.hpp,free_shape_bounds.hpp,shape_certificate.hpp,test.cpp,README.md} bindings/common/distributions/ged/fit.hpp bindings/r/distributions/ged/test.R bindings/js/test.mjs
just cpp-test
UMVL_TEST_TIER=full just cpp-test
just cpp-test-no-long-double
just test
just test-full
