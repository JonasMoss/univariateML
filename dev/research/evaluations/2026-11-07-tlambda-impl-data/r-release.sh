#!/usr/bin/env bash
set -euo pipefail
mkdir -p build/scratch
sha256sum univariateML-cpp/include/cppunivariate/distributions/tlambda/{mle,certificate}.hpp bindings/common/distributions/tlambda/fit.hpp bindings/r/distributions/tlambda/test.R univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md > build/scratch/r-source.before
just test
just test-full
sha256sum univariateML-cpp/include/cppunivariate/distributions/tlambda/{mle,certificate}.hpp bindings/common/distributions/tlambda/fit.hpp bindings/r/distributions/tlambda/test.R univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md > build/scratch/r-source.after
cmp build/scratch/r-source.before build/scratch/r-source.after
cat build/scratch/r-source.after
