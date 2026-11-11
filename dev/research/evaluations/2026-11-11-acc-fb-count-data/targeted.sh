#!/usr/bin/env bash
set -euo pipefail
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
sources=()
for id in bbinom tbinom ztbinom zib zmbinom; do sources+=("univariateML-cpp/include/cppunivariate/distributions/$id/test.cpp"); done
c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" \
  -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" \
  univariateML-cpp/tests/distribution_tests_main.cpp "${sources[@]}" -o build/scratch/count-tests
build/scratch/count-tests
