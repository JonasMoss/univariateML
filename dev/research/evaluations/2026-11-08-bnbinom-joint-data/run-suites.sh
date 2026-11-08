#!/bin/sh
# Invoke through simbox-run, or run4 after exit 75. No nested slot locks.
set -eu
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/bnbinom-joint
for suite in oracle targeted cpp-dev cpp-full cpp-no-long-double r-dev r-full; do
  echo "START $suite $(date -u +%FT%TZ)"
  case "$suite" in
    oracle) python3 dev/research/evaluations/2026-11-08-bnbinom-joint-data/oracle.py;;
    targeted) sh dev/research/evaluations/2026-11-08-bnbinom-joint-data/targeted.sh;;
    cpp-dev) just cpp-test;;
    cpp-full) UMVL_TEST_TIER=full just cpp-test;;
    cpp-no-long-double) just cpp-test-no-long-double;;
    r-dev) just test;;
    r-full) just test-full;;
  esac
  echo "GREEN $suite $(date -u +%FT%TZ)"
done
