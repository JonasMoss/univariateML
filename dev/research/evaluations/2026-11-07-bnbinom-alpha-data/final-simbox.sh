#!/bin/sh
set -eu
c++ --version | head -1
Rscript --version
sha256sum univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp bindings/common/distributions/bnbinom/fit.hpp univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp bindings/r/distributions/bnbinom/test.R
sh dev/research/evaluations/2026-11-07-bnbinom-alpha-data/run-suites.sh
sha256sum univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp bindings/common/distributions/bnbinom/fit.hpp univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp bindings/r/distributions/bnbinom/test.R
