#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export MAKEFLAGS=-j3 TESTTHAT_CPUS=1
export UMVL_DISTRIBUTIONS_DIR=$PWD/univariateML-cpp/include/cppunivariate/distributions
mkdir -p build/tmp build/scratch
just regen
cd univariateML-r
UMVL_TEST_TIER=full Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="^dist-sged$",stop_on_failure=TRUE)'
