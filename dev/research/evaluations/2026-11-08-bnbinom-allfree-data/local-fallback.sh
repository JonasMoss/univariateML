#!/bin/sh
# Invoke through run4 after simbox exit 75; no nested CPU reservations.
set -eu
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/bnbinom-allfree
python3 dev/research/evaluations/2026-11-08-bnbinom-allfree-data/audit.py
for suite in cpp-dev cpp-full cpp-no-long-double test test-full; do
  echo "START workstation-fallback-$suite $(date -u +%FT%TZ)"
  case "$suite" in
    cpp-dev) just cpp-test;;
    cpp-full) UMVL_TEST_TIER=full just cpp-test;;
    cpp-no-long-double) just cpp-test-no-long-double;;
    test) just test;;
    test-full) just test-full;;
  esac
  echo "GREEN workstation-fallback-$suite $(date -u +%FT%TZ)"
done
python3 dev/research/evaluations/2026-11-08-bnbinom-allfree-data/audit.py
