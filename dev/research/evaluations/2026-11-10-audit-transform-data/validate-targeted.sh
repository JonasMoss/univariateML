#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
files=()
for id in invgamma lgamma naka lnorm logitnorm llogis invweibull invburr; do files+=("univariateML-cpp/include/cppunivariate/distributions/$id/test.cpp"); done
c++ -std=c++17 -O0 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" \
  -DCPPUNIV_DISTRIBUTIONS_DIR=\""$PWD/univariateML-cpp/include/cppunivariate/distributions"\" \
  univariateML-cpp/tests/distribution_tests_main.cpp "${files[@]}" -o build/scratch/transform-tests
build/scratch/transform-tests
