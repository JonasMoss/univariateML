#!/bin/bash
set -eu
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}" CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1 CCACHE_MAXSIZE=8G
export LC_ALL=C.UTF-8 TZ=UTC
D=dev/research/evaluations/2026-11-14-core5b-c-data
just regen
cd univariateML-r
UMVL_TEST_TIER=full TESTTHAT_CPUS=4 PKG_BUILD_EXTRA_FLAGS=false Rscript -e 'options(pkg.build_extra_flags=FALSE); testthat::test_local(".",filter="^dist-(gb2|fpareto|genbeta)$",stop_on_failure=TRUE)'
