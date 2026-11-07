#!/usr/bin/env bash
set -euo pipefail
just regen
cd univariateML-r
UMVL_DISTRIBUTIONS_DIR="$PWD/../univariateML-cpp/include/cppunivariate/distributions" \
PKG_BUILD_EXTRA_FLAGS=false UMVL_TEST_TIER=dev Rscript -e \
 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="vcov",stop_on_failure=TRUE)'
