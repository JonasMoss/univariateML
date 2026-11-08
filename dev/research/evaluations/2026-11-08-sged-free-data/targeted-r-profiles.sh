#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp MAKEFLAGS=-j3 TESTTHAT_CPUS=1
export UMVL_DISTRIBUTIONS_DIR=$PWD/univariateML-cpp/include/cppunivariate/distributions
mkdir -p build/tmp build/scratch
just regen
python3 - <<'PYR'
from pathlib import Path
s=Path('bindings/r/distributions/sged/test.R').read_text()
s=s[s.index('test_that("SGED asymmetric held skew and smooth free skew'):]
Path('univariateML-r/tests/testthat/test-sged-profiles.R').write_text(s)
PYR
profile_test_file=$PWD/univariateML-r/tests/testthat/test-sged-profiles.R
trap 'rm -f "$profile_test_file"' EXIT
cd univariateML-r
UMVL_TEST_TIER=full Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="^sged-profiles$",stop_on_failure=TRUE)'
