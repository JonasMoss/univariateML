#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
just regen
cd univariateML-r
UMVL_TEST_TIER=dev TESTTHAT_CPUS=4 PKG_BUILD_EXTRA_FLAGS=false Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="^(dist-(gpd|pareto4)|conformance)$",stop_on_failure=TRUE)'
