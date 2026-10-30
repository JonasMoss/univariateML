#!/usr/bin/env bash
# Final native checks; run under run4, with temporary files in this worktree.
set -euo pipefail
cd "$(dirname "$0")/../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
bash bindings/js/check-native-seam.sh > build/scratch/native-seam.log 2>&1
just cpp-test > build/scratch/cpp-test.log 2>&1
NOT_CRAN=true just test > build/scratch/r-test.log 2>&1
just js-build > build/scratch/js-final-build.log 2>&1
just js-test > build/scratch/js-final-test.log 2>&1
