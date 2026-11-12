#!/usr/bin/env bash
# Fresh, optimized native binary for the only late-fix family. No stale PCH.
set -euo pipefail
cd "$(dirname "$0")/../../../.."
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}"
export CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1 CCACHE_MAXSIZE=8G
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
flags=(); mode=native
if [[ ${UMVL_RETEST_NO_LONG_DOUBLE:-0} == 1 ]]; then
  flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); mode=double
fi
ccache c++ -std=gnu++17 -Wall -Wextra -Wpedantic -g -O2 -DNDEBUG \
  "${flags[@]}" \
  "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"" \
  -IunivariateML-cpp/tests -IunivariateML-cpp/include "-I$boost" \
  univariateML-cpp/tests/distribution_tests_main.cpp \
  univariateML-cpp/include/cppunivariate/distributions/binom/test.cpp \
  -o "build/scratch/seam-rerun/binom-$mode-retest"
"build/scratch/seam-rerun/binom-$mode-retest"
