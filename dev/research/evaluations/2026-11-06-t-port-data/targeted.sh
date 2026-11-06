#!/bin/sh
set -eu
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" \
  -DCPPUNIV_DISTRIBUTIONS_DIR=\""$PWD/univariateML-cpp/include/cppunivariate/distributions"\" \
  univariateML-cpp/tests/distribution_tests_main.cpp \
  univariateML-cpp/include/cppunivariate/distributions/lst/test.cpp \
  univariateML-cpp/include/cppunivariate/distributions/std/test.cpp \
  -o build/scratch/t-port-targeted
build/scratch/t-port-targeted '--source-file=*distributions/lst/test.cpp,*distributions/std/test.cpp'
