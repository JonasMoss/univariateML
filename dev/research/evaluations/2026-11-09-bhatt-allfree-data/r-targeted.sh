#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
just regen
cd univariateML-r
export UMVL_DISTRIBUTIONS_DIR=$OLDPWD/univariateML-cpp/include/cppunivariate/distributions
export UMVL_TEST_TIER=full PKG_BUILD_EXTRA_FLAGS=false TESTTHAT_CPUS=1
Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="dist-bhatt",stop_on_failure=TRUE)'
