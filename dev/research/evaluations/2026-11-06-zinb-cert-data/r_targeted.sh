#!/usr/bin/env bash
set -euo pipefail
just regen
export UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions"
export UMVL_TEST_TIER=full PKG_BUILD_EXTRA_FLAGS=false TESTTHAT_CPUS=1
cd univariateML-r
Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".", filter="dist-zinb", stop_on_failure=TRUE)'
