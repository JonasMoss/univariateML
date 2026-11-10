#!/usr/bin/env bash
set -euo pipefail
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
cat > build/scratch/bhatt-main.cpp <<'CPP'
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
CPP
extra=()
if [[ ${1:-} == double ]]; then extra=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); fi
"${CXX:-g++}" -O2 -std=c++17 -Wall -Wextra "${extra[@]}" \
  -DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\" \
  -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" \
  build/scratch/bhatt-main.cpp univariateML-cpp/include/cppunivariate/distributions/bhatt/test.cpp \
  -o build/scratch/bhatt-targeted
build/scratch/bhatt-targeted
