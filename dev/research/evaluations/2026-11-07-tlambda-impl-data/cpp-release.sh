#!/usr/bin/env bash
set -euo pipefail
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1 CCACHE_MAXSIZE=8G
mode=${1:-ordinary}
if [ "$mode" = no-long-double ]; then
  tree=build/cpp-no-long-double
  recipe=cpp-test-no-long-double
else
  tree=univariateML-cpp/build
  recipe=cpp-test
fi
mkdir -p build/scratch
if [ ! -f "$tree/CMakeCache.txt" ]; then UMVL_TEST_TIER=dev just "$recipe"; fi
sha256sum univariateML-cpp/include/cppunivariate/distributions/tlambda/{mle,certificate}.hpp bindings/common/distributions/tlambda/fit.hpp univariateML-cpp/include/cppunivariate/distributions/tlambda/test.cpp > "build/scratch/$mode-source.before"
# Remove objects from earlier development snapshots before the final suite.
cmake --build "$tree" --clean-first -j "${CMAKE_BUILD_PARALLEL_LEVEL:-3}"
UMVL_TEST_TIER=full just "$recipe"
if [ "$mode" = ordinary ]; then
  ctest --test-dir "$tree" --output-on-failure -j "${CTEST_PARALLEL_LEVEL:-3}" -L dev
fi
sha256sum univariateML-cpp/include/cppunivariate/distributions/tlambda/{mle,certificate}.hpp bindings/common/distributions/tlambda/fit.hpp univariateML-cpp/include/cppunivariate/distributions/tlambda/test.cpp > "build/scratch/$mode-source.after"
cmp "build/scratch/$mode-source.before" "build/scratch/$mode-source.after"
cat "build/scratch/$mode-source.after"
