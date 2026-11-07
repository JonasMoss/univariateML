#!/usr/bin/env bash
set -euo pipefail
just regen
cd univariateML-r
export UMVL_DISTRIBUTIONS_DIR="$PWD/../univariateML-cpp/include/cppunivariate/distributions"
export UMVL_TEST_TIER=full TESTTHAT_CPUS=1 PKG_BUILD_EXTRA_FLAGS=false
Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="^dist-tlambda$",stop_on_failure=TRUE)'
