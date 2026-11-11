#!/bin/bash
# Run on simbox after the full tiers; only the late-changed family is rerun.
set -euo pipefail
export CCACHE_DIR=${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML} CCACHE_BASEDIR=$PWD CCACHE_NOHASHDIR=1
# A simbox drop can route this check to a worktree whose native build is new.
configure_target_build() {
 local test_build_dir=$1
 shift
 if [ ! -f "$test_build_dir/CMakeCache.txt" ];then
  local boost_headers_r
  boost_headers_r=$(Rscript -e 'cat(system.file("include",package="BH"))')
  cmake -S univariateML-cpp -B "$test_build_dir" -DCMAKE_CXX_COMPILER_LAUNCHER=ccache -DBoost_INCLUDE_DIR="$boost_headers_r" "$@"
 fi
}
configure_target_build univariateML-cpp/build
configure_target_build build/cpp-no-long-double -DCMAKE_CXX_FLAGS=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
cmake --build univariateML-cpp/build --target distribution_tests -j4
ctest --test-dir univariateML-cpp/build --output-on-failure -R '^distribution_sged(_slow)?$' -j2
cmake --build build/cpp-no-long-double --target distribution_tests -j4
ctest --test-dir build/cpp-no-long-double --output-on-failure -R '^distribution_sged(_slow)?$' -j2
just regen
cd univariateML-r
UMVL_TEST_TIER=full UMVL_DISTRIBUTIONS_DIR="$PWD/../univariateML-cpp/include/cppunivariate/distributions" PKG_BUILD_EXTRA_FLAGS=false TESTTHAT_CPUS=1 Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".", filter="^dist-sged$", stop_on_failure=TRUE)'
