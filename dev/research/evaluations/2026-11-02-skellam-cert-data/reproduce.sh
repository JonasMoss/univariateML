#!/usr/bin/env bash
# Run from the repository root under /tmp/umvl-cpu/run.
set -euo pipefail
mkdir -p build/scratch
git show 6fea876e:univariateML-cpp/include/cppunivariate/distributions/skellam/mle.hpp |
  sed -e 's|"../../core/precision.hpp"|"cppunivariate/core/precision.hpp"|' \
      -e 's|"../../core/span.hpp"|"cppunivariate/core/span.hpp"|' \
      -e 's|"dpqr.hpp"|"cppunivariate/distributions/skellam/dpqr.hpp"|' > build/scratch/old.hpp
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -Ibuild/scratch -I"$boost" \
  dev/research/evaluations/2026-11-02-skellam-cert-data/compare.cpp -o build/scratch/skellam-compare
build/scratch/skellam-compare
