#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p build/tmp build/scratch
sha256sum -c dev/research/evaluations/2026-11-08-tlambda-speed-impl-data/source-manifest.sha256
just cpp-test > build/scratch/final-cpp-dev.log 2>&1
just cpp-test-no-long-double > build/scratch/final-cpp-no-long-double.log 2>&1
just test > build/scratch/final-r-dev.log 2>&1
just test-full > build/scratch/final-r-full.log 2>&1
sha256sum -c dev/research/evaluations/2026-11-08-tlambda-speed-impl-data/source-manifest.sha256
for f in build/scratch/final-cpp-dev.log build/scratch/final-cpp-no-long-double.log build/scratch/final-r-dev.log build/scratch/final-r-full.log;do tail -15 "$f";done
