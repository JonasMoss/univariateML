#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
ctest --test-dir univariateML-cpp/build --output-on-failure -R '^distribution_genbeta_slow$' -j1
ctest --test-dir build/cpp-no-long-double --output-on-failure -R '^distribution_genbeta_slow$' -j1
