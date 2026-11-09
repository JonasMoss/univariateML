#!/bin/sh
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=1 CTEST_PARALLEL_LEVEL=1
mkdir -p build/tmp build/scratch/bnbinom-face
for suite in cpp-dev cpp-full cpp-no-long-double; do
  echo "START simbox $suite $(date -u +%FT%TZ)"
  case "$suite" in
    cpp-dev) just cpp-test;;
    cpp-full) UMVL_TEST_TIER=full just cpp-test;;
    cpp-no-long-double) just cpp-test-no-long-double;;
  esac
  echo "GREEN simbox $suite $(date -u +%FT%TZ)"
done
