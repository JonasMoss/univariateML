#!/bin/sh
# Run from the worktree root via simbox-run, or run4 after exit 75.
set -eu
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p build/tmp build/scratch/bnbinom-alpha
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
for suite in targeted cpp-dev cpp-full cpp-no-long-double r-dev r-full; do
  echo "START $suite $(date -u +%FT%TZ)"
  case "$suite" in
    targeted) sh dev/research/evaluations/2026-11-07-bnbinom-alpha-data/targeted.sh;;
    cpp-dev) just cpp-test;;
    cpp-full) UMVL_TEST_TIER=full just cpp-test;;
    cpp-no-long-double) just cpp-test-no-long-double;;
    r-dev) just test;;
    r-full) just test-full;;
  esac
  echo "GREEN $suite $(date -u +%FT%TZ)"
done
