#!/bin/bash
# Execute inside simbox-run; the wrapper supplies nice and thread pinning.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 MAKEFLAGS=-j4
mkdir -p build/tmp build/scratch
c++ --version | head -1
Rscript --version
python3 --version
sha256sum univariateML-cpp/include/cppunivariate/distributions/sged/{mle.hpp,held_moment_certificate.hpp,README.md,test.cpp,golden.json} bindings/r/distributions/sged/test.R > build/scratch/validated-sources.sha256
/usr/bin/time -f 'TARGETED_SECONDS=%e' bash dev/research/evaluations/2026-11-08-sged-held-data/validate-targeted.sh
/usr/bin/time -f 'CPP_DEV_SECONDS=%e' env UMVL_TEST_TIER=dev just cpp-test
/usr/bin/time -f 'R_DEV_SECONDS=%e' env UMVL_TEST_TIER=dev just test
/usr/bin/time -f 'CPP_FULL_SECONDS=%e' env UMVL_TEST_TIER=full just cpp-test
/usr/bin/time -f 'R_FULL_SECONDS=%e' just test-full
/usr/bin/time -f 'CPP_NO_LONG_DOUBLE_SECONDS=%e' just cpp-test-no-long-double
# The workstation has the pre-provisioned Node tree and Git metadata; simbox
# deliberately receives neither. Keep the local finish in the same reservation.
if [[ -d site/node_modules && -e .git ]]; then
  SGED_SKIP_NO_LONG_DOUBLE=1 bash dev/research/evaluations/2026-11-08-sged-held-data/validate-local.sh
  /usr/bin/time -f 'JS_TARGETED_SECONDS=%e' node --v8-pool-size=1 dev/research/evaluations/2026-11-08-sged-held-data/targeted-js.mjs
  python3 dev/research/evaluations/2026-11-08-sged-held-data/collect_validation.py
fi
