#!/bin/sh
set -eu
mkdir -p build/tmp build/scratch/bnbinom-joint
export TMPDIR="$PWD/build/tmp"
bh=$(Rscript -e 'cat(system.file("include",package="BH"))')
printf '#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN\n#include "doctest.h"\n' > build/scratch/bnbinom-joint/main.cpp
c++ -O2 -std=c++17 -Wall -Wextra -Wpedantic -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$bh" \
  -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" \
  build/scratch/bnbinom-joint/main.cpp univariateML-cpp/include/cppunivariate/distributions/bnbinom/test.cpp -o build/scratch/bnbinom-joint/tests
build/scratch/bnbinom-joint/tests
