#!/bin/sh
# Exactly one suite per invocation. Use run4 locally or simbox-run remotely.
set -eu
machine=$1
suite=$2
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p build/tmp build/scratch/bnbinom-allfree
printf 'START %s-%s %s\n' "$machine" "$suite" "$(date -u +%FT%TZ)"
python3 dev/research/evaluations/2026-11-08-bnbinom-allfree-data/audit.py
case "$suite" in
  cpp-dev) just cpp-test;;
  cpp-full) UMVL_TEST_TIER=full just cpp-test;;
  cpp-no-long-double) just cpp-test-no-long-double;;
  test) just test;;
  test-full) just test-full;;
  js-build) just js-build;;
  js-test) just js-test;;
  site-check) just site-check;;
  *) exit 2;;
esac
python3 dev/research/evaluations/2026-11-08-bnbinom-allfree-data/audit.py
printf 'GREEN %s-%s %s\n' "$machine" "$suite" "$(date -u +%FT%TZ)"
