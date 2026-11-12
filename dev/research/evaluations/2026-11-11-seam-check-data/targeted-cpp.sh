#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
mkdir -p build/scratch/seam-check
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
sources=(univariateML-cpp/tests/distribution_tests_main.cpp)
ids=("$@")
if [ "${#ids[@]}" = 0 ]; then ids=(bbinom beta binom invgauss nbinom nsbeta tnorm tpois zmbinom ztbinom); fi
for id in "${ids[@]}"; do
  sources+=("univariateML-cpp/include/cppunivariate/distributions/$id/test.cpp")
done
flags=()
suffix=""
if [ "${UMVL_SEAM_NO_LONG_DOUBLE:-0}" = 1 ]; then flags=(-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS); suffix="-double"; fi
ccache c++ -std=c++17 -O1 "${flags[@]}" -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" \
  "-DCPPUNIV_DISTRIBUTIONS_DIR=\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"" \
  "${sources[@]}" -o "build/scratch/seam-check/targeted-tests$suffix"
"build/scratch/seam-check/targeted-tests$suffix" \
  '--test-case-exclude=*oracle*,*multistart*,*replication*,*large sample*,*large-sample*,*every held golden subset*,*slow*reference*,*240 seeded*'
