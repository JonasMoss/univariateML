#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
mkdir -p "$TMPDIR"
just cpp-test-no-long-double
just js-build
just js-test
just site-check
bash dev/research/evaluations/2026-11-07-evt-joint-data/benchmark.sh
python3 dev/research/evaluations/2026-11-07-evt-joint-data/bound-scales.py > dev/research/evaluations/2026-11-07-evt-joint-data/bound-scales.json
