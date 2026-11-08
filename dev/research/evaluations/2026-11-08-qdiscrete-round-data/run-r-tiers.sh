#!/usr/bin/env bash
# Same generation and runner as just test / just test-full, one worker so the
# independent C++ runs and the workstation queue can continue within six cores.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export MAKEFLAGS=-j1 TESTTHAT_CPUS="${TESTTHAT_CPUS:-1}" PKG_BUILD_EXTRA_FLAGS=false
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
export UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions"
mkdir -p build/tmp build/scratch
test_tiers=("$@")
if [ "${#test_tiers[@]}" -eq 0 ]; then test_tiers=(dev); fi
just regen
cd univariateML-r
for test_tier in "${test_tiers[@]}"; do
  UMVL_TEST_TIER="$test_tier" Rscript ../bindings/r/scripts/test_local.R
done
