#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p build/tmp build/scratch
just cpp-test > build/scratch/final-cpp-dev.log 2>&1
just cpp-test-no-long-double > build/scratch/final-cpp-no-long-double.log 2>&1
just test > build/scratch/final-r-dev.log 2>&1
for f in build/scratch/final-cpp-dev.log build/scratch/final-cpp-no-long-double.log build/scratch/final-r-dev.log; do tail -12 "$f"; done
