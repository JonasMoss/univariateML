#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p build/tmp build/scratch
run_suite() {
  local label=$1
  shift
  if "$@" > "build/scratch/final-$label.log" 2>&1; then
    cat "build/scratch/final-$label.log"
  else
    cat "build/scratch/final-$label.log"
    return 1
  fi
}
run_suite cpp-dev just cpp-test
run_suite cpp-full env UMVL_TEST_TIER=full just cpp-test
run_suite cpp-no-long-double just cpp-test-no-long-double
run_suite r-dev just test
run_suite r-full just test-full
