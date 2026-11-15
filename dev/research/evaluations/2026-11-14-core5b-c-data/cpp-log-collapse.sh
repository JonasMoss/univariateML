#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1 CCACHE_MAXSIZE=8G
export LC_ALL=C.UTF-8 TZ=UTC
D=dev/research/evaluations/2026-11-14-core5b-c-data
cmake --build univariateML-cpp/build --target distribution_tests -j4
ctest --test-dir univariateML-cpp/build --output-on-failure -L full -R '^distribution_(gb2|fpareto|genbeta)(_slow)?$' -j4
cmake --build build/cpp-no-long-double --target distribution_tests -j4
ctest --test-dir build/cpp-no-long-double --output-on-failure -L full -R '^distribution_(gb2|fpareto|genbeta)(_slow)?$' -j4
