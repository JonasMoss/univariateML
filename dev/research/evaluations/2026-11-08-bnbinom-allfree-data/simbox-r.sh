#!/bin/sh
set -eu
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p build/tmp build/scratch/bnbinom-allfree
python3 dev/research/evaluations/2026-11-08-bnbinom-allfree-data/audit.py
for suite in test test-full; do
  echo "START simbox-$suite $(date -u +%FT%TZ)"
  just "$suite"
  echo "GREEN simbox-$suite $(date -u +%FT%TZ)"
done
python3 dev/research/evaluations/2026-11-08-bnbinom-allfree-data/audit.py
