#!/usr/bin/env bash
# Run this sequential build and test through /tmp/umvl-cpu/run.
set -euo pipefail
export TMPDIR=$PWD/build/tmp
b=$(Rscript -e 'cat(system.file("include",package="BH"))')
for mode in native double; do
  flags=()
  [ "$mode" != double ] || flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS)
  g++ -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$b" \
    "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"" \
    "${flags[@]}" univariateML-cpp/tests/distribution_tests_main.cpp \
    univariateML-cpp/include/cppunivariate/distributions/gev/test.cpp -o "build/scratch/held2-gev-$mode"
  "build/scratch/held2-gev-$mode" --test-case='gev:*'
done
