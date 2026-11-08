#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
boost_dir=$(Rscript -e 'cat(system.file("include",package="BH"))')
cmake -S univariateML-cpp -B univariateML-cpp/build -DCMAKE_CXX_COMPILER_LAUNCHER=ccache -DBoost_INCLUDE_DIR="$boost_dir"
cmake --build univariateML-cpp/build --target distribution_tests -j 3
ctest --test-dir univariateML-cpp/build --output-on-failure -R '^distribution_ged(_slow)?$'
