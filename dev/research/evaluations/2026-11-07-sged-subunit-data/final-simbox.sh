#!/usr/bin/env bash
# Invoke via /tmp/umvl-cpu/simbox-run bash <this file>.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=1 CTEST_PARALLEL_LEVEL=1
mkdir -p "$TMPDIR"
date -u
uname -sr
sha256sum -c dev/research/evaluations/2026-11-07-sged-subunit-data/source-manifest.sha256
g++ --version | head -1
R --version | head -1
Rscript -e 'cat("BH ",as.character(packageVersion("BH")),"; Rcpp ",as.character(packageVersion("Rcpp")),"\n",sep="")'
/usr/bin/time -p just cpp-test
/usr/bin/time -p env UMVL_TEST_TIER=full just cpp-test
/usr/bin/time -p just cpp-test-no-long-double
just regen
(cd univariateML-r && UMVL_DISTRIBUTIONS_DIR="$OLDPWD/univariateML-cpp/include/cppunivariate/distributions" UMVL_TEST_TIER=dev TESTTHAT_CPUS=1 PKG_BUILD_EXTRA_FLAGS=false Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="^sged-subunit$",stop_on_failure=TRUE)')
/usr/bin/time -p just test
/usr/bin/time -p just test-full
date -u
