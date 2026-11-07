#!/bin/sh
# Run using /tmp/umvl-cpu/simbox-run sh <this-file>.
set -eu
cmake --build univariateML-cpp/build --target distribution_tests -j "${CMAKE_BUILD_PARALLEL_LEVEL:-1}"
ctest --test-dir univariateML-cpp/build --output-on-failure -R distribution_betapr
just regen
dossiers="$PWD/univariateML-cpp/include/cppunivariate/distributions"
cd univariateML-r
UMVL_DISTRIBUTIONS_DIR="$dossiers" UMVL_TEST_TIER=full TESTTHAT_CPUS=1 PKG_BUILD_EXTRA_FLAGS=false \
 Rscript -e 'options(pkg.build_extra_flags=FALSE);testthat::test_local(".",filter="dist-betapr",stop_on_failure=TRUE)'
