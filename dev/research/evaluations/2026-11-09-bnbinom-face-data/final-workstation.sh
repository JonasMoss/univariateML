#!/bin/sh
# Invoke through run4. All suites remain foreground and run sequentially.
set -eu
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p build/tmp build/scratch/bnbinom-face
for suite in cpp-dev cpp-full cpp-no-long-double test test-full; do
  log="build/scratch/bnbinom-face/$suite-final.log"
  echo "START workstation $suite $(date -u +%FT%TZ)"
  case "$suite" in
    cpp-dev) just cpp-test > "$log" 2>&1;;
    cpp-full) UMVL_TEST_TIER=full just cpp-test > "$log" 2>&1;;
    cpp-no-long-double) just cpp-test-no-long-double > "$log" 2>&1;;
    test) just test > "$log" 2>&1;;
    test-full) just test-full > "$log" 2>&1;;
  esac
  echo "GREEN workstation $suite $(date -u +%FT%TZ)"
done
