#!/bin/sh
set -eu
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
c++ -O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" \
  -DCPPUNIV_DISTRIBUTIONS_DIR=\""$PWD/univariateML-cpp/include/cppunivariate/distributions"\" \
  univariateML-cpp/tests/distribution_tests_main.cpp \
  univariateML-cpp/include/cppunivariate/distributions/lst/test.cpp \
  univariateML-cpp/include/cppunivariate/distributions/std/test.cpp \
  -o build/scratch/t-close-targeted
build/scratch/t-close-targeted '--test-case-exclude=*large-sample*'
