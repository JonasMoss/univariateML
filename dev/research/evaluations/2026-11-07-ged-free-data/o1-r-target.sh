#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
just regen
export UMVL_DISTRIBUTIONS_DIR=$PWD/univariateML-cpp/include/cppunivariate/distributions
cd univariateML-r
UMVL_TEST_TIER=full TESTTHAT_CPUS=1 PKG_BUILD_EXTRA_FLAGS=false Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="dist-ged",stop_on_failure=TRUE)'
