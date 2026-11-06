#!/bin/sh
# Run from the worktree root, through simbox-run or the local run4 lock.
set -eu
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
for suite in cpp-dev cpp-full cpp-no-long-double r-dev r-full; do
  echo "START $suite $(date -u +%FT%TZ)"
  case "$suite" in
    cpp-dev) just cpp-test;;
    cpp-full) UMVL_TEST_TIER=full just cpp-test;;
    cpp-no-long-double) just cpp-test-no-long-double;;
    r-dev) just test;;
    r-full) just test-full;;
  esac
  echo "GREEN $suite $(date -u +%FT%TZ)"
done
