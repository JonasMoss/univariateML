#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export EMCC_CORES=4
mkdir -p "$TMPDIR"
just cpp-test-no-long-double
just js-build
just js-test
just site-check
