#!/bin/bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
c++ -std=c++17 -O2 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" univariateML-cpp/tests/distribution_tests_main.cpp univariateML-cpp/include/cppunivariate/distributions/gev/test.cpp univariateML-cpp/include/cppunivariate/distributions/frechet/test.cpp -o build/scratch/perf-target
build/scratch/perf-target
