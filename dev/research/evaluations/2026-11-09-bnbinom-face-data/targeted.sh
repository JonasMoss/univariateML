#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/bnbinom-face
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
for mode in native double; do
  flags=''
  if [ "$mode" = double ]; then flags=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS; fi
  c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic $flags -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$bh" \
    -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" \
    univariateML-cpp/tests/distribution_tests_main.cpp univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp \
    -o build/scratch/bnbinom-face/tests-$mode
  build/scratch/bnbinom-face/tests-$mode
 done
