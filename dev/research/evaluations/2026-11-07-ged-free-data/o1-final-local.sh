#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
c++ --version | head -1
sha256sum univariateML-cpp/include/cppunivariate/distributions/ged/{mle.hpp,free_shape_bounds.hpp,shape_certificate.hpp,test.cpp,README.md} bindings/common/distributions/ged/fit.hpp bindings/r/distributions/ged/test.R bindings/js/test.mjs
just cpp-test-no-long-double
just js-build
just js-test
just site-check
