#!/bin/bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 MAKEFLAGS=-j4 TESTTHAT_CPUS=1
mkdir -p build/tmp build/scratch
boost=$(Rscript -e 'cat(system.file("include",package="BH"))')
data=dev/research/evaluations/2026-11-08-sged-held-data
c++ -O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" "$data/check.cpp" -o build/scratch/sged-check
build/scratch/sged-check > "$data/checks.tsv"
python3 "$data/check_math.py"
c++ -O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" "$data/probe.cpp" -o build/scratch/sged-after
build/scratch/sged-after > "$data/after.tsv"
c++ -O2 -std=c++17 -IunivariateML-cpp/include -IunivariateML-cpp/tests -I"$boost" -DCPPUNIV_DISTRIBUTIONS_DIR="\"$PWD/univariateML-cpp/include/cppunivariate/distributions\"" univariateML-cpp/tests/distribution_tests_main.cpp univariateML-cpp/include/cppunivariate/distributions/sged/test.cpp -o build/scratch/sged-tests
build/scratch/sged-tests
just regen
export UMVL_DISTRIBUTIONS_DIR=$PWD/univariateML-cpp/include/cppunivariate/distributions
cd univariateML-r
UMVL_TEST_TIER=full Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="^dist-sged$",stop_on_failure=TRUE)'
