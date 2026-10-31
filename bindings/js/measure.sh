#!/usr/bin/env bash
# Reserve all four CPU slots before invoking; builds remain sequential.
# Pilot optimization/native timings remain in the 2026-10-30 report/base revision.
set -euo pipefail
cd "$(dirname "$0")/../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
JS_FAMILIES=exp bash bindings/js/build.sh O2 exp-only > build/scratch/js-exp-build.log 2>&1
JS_FAMILIES=gb2 bash bindings/js/build.sh O2 gb2-only > build/scratch/js-gb2-build.log 2>&1
unset JS_FAMILIES
just js-build > build/scratch/js-final-build.log 2>&1
node --v8-pool-size=1 bindings/js/measure-all.mjs exp-only gb2-only univariateML > build/scratch/module-layout.log 2>&1
just js-test > build/scratch/js-final-test.log 2>&1
