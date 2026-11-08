#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
c++ --version | head -1
sha256sum univariateML-cpp/include/cppunivariate/distributions/sged/{mle.hpp,free_shape_certificate.hpp,profile_bounds.hpp,weighted_shape_certificate.hpp,mode_skew_certificate.hpp,test.cpp,README.md,golden.json} bindings/common/distributions/sged/fit.hpp bindings/js/test.mjs
just cpp-test-no-long-double
