#!/bin/bash
# Copy a tree to simbox and run cpp-test, test and quality there.
# usage: scripts/simbox-test.sh <label> [tree]   (tree defaults to this checkout)
# simbox setup (2026-10-29): gcc-15/g++-15/gfortran-15 (toolchain PPA), cmake 4
# (Kitware), just 1.45 in /usr/local/bin, BH 1.90 (no system Boost), R packages
# in ~/R; ~/.R/Makevars routes R builds through ccache g++-15; ~/.umvl-env sets
# CC/CXX for cmake.
label=${1:?label}; src=${2:-$(cd "$(dirname "$0")/.." && pwd)}
dst=/home/jmoss/umvl/$label
ssh simbox "mkdir -p $dst"
rsync -a --delete --exclude .git --exclude /univariateML/package --exclude /cppunivariate/build \
  --exclude /build --exclude Rplots.pdf "$src"/ simbox:$dst/ || exit 1
ssh simbox "cd $dst && . ~/.umvl-env && export MAKEFLAGS=-j8 CMAKE_BUILD_PARALLEL_LEVEL=8 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1;
  nice -n 10 just cpp-test > cpp.log 2>&1; echo CPP_EXIT=\$?;
  grep -E 'tests passed|tests failed' cpp.log | tail -1; grep -E 'ERROR' cpp.log | head -10;
  nice -n 10 just test > r.log 2>&1; echo R_EXIT=\$?;
  grep -E '^\[ FAIL' r.log | tail -1; grep -E '(Failure|Error) \(' r.log | sort -u | head -20;
  nice -n 10 just quality > q.log 2>&1; echo Q_EXIT=\$?; grep -vc ' pass' q.log"
