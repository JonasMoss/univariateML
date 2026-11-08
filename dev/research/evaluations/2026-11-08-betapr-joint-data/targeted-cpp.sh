#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
boost_path=$(Rscript -e 'cat(system.file("include",package="BH"))')
flags=(-std=c++17 -O1 -IunivariateML-cpp/include -IunivariateML-cpp/tests "-I$boost_path" "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"")
for mode in native double; do
  extra=()
  if [ "$mode" = double ]; then extra=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); fi
  c++ "${flags[@]}" "${extra[@]}" univariateML-cpp/tests/distribution_tests_main.cpp univariateML-cpp/include/cppunivariate/distributions/betapr/test.cpp -o "build/scratch/betapr-tests-$mode"
  "build/scratch/betapr-tests-$mode"
done
