#!/bin/sh
set -eu
export XDG_RUNTIME_DIR="$TMPDIR"
export MAKEFLAGS=-j3 CMAKE_BUILD_PARALLEL_LEVEL=3
just regen
export UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions"
export UMVL_TEST_TIER="${UMVL_TEST_TIER:-dev}" PKG_BUILD_EXTRA_FLAGS=false TESTTHAT_CPUS=3
cd univariateML-r
Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="dist-(std|lst)$",stop_on_failure=TRUE)'
