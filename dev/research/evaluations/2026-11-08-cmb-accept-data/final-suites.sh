#!/usr/bin/env bash
# Invoked through simbox-run, or through run4 after exit 75.
set -euo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
sha256sum -c dev/research/evaluations/2026-11-08-cmb-accept-data/source.sha256
just cpp-test
UMVL_TEST_TIER=full just cpp-test
just cpp-test-no-long-double
just test
just test-full
