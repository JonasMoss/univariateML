#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions"
mkdir -p "$TMPDIR"
just regen
cd univariateML-r
UMVL_TEST_TIER=full TESTTHAT_CPUS=1 PKG_BUILD_EXTRA_FLAGS=false Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="^dist-betapr$",stop_on_failure=TRUE)'
