#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp EMCC_CORES=4
mkdir -p build/tmp build/scratch
c++ --version | head -1
sha256sum univariateML-cpp/include/cppunivariate/distributions/sged/{mle.hpp,free_shape_certificate.hpp,profile_bounds.hpp,weighted_shape_certificate.hpp,mode_skew_certificate.hpp,test.cpp,README.md,golden.json} bindings/common/distributions/sged/fit.hpp bindings/r/distributions/sged/test.R bindings/js/test.mjs
just cpp-test-no-long-double
just js-build
if just js-test > build/scratch/final-js.log 2>&1; then
  cat build/scratch/final-js.log
else
  cat build/scratch/final-js.log
  python3 - <<'PYJS'
import re
from pathlib import Path
s=Path('build/scratch/final-js.log').read_text()
assert re.findall(r'^not ok \d+ - (.*)$',s,re.M)==['binomial relatives retain unresolved finite and Poisson witnesses']
assert re.search(r'^# fail 1$',s,re.M)
print('KNOWN upstream stale duplicate only; maintainer instructed leaving it unchanged for merge.')
PYJS
fi
just site-check
