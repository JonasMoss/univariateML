#!/bin/bash
set -euo pipefail
just regen
export UMVL_DISTRIBUTIONS_DIR=$PWD/univariateML-cpp/include/cppunivariate/distributions
export UMVL_TEST_TIER=dev PKG_BUILD_EXTRA_FLAGS=false TESTTHAT_CPUS=3
Rscript -e 'testthat::test_local("univariateML-r", filter="^dist-(ztbinom|zmbinom|tbinom|zib)$", stop_on_failure=TRUE)'
