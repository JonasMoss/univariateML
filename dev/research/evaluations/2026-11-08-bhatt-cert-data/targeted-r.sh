#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
export UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions"
export MAKEFLAGS=-j3
export UMVL_TEST_TIER=full TESTTHAT_CPUS=1 PKG_BUILD_EXTRA_FLAGS=false
mkdir -p build/tmp build/scratch
just regen > build/scratch/bhatt-regen.log 2>&1
cd univariateML-r
Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".", filter="^dist-bhatt$", stop_on_failure=TRUE)'
