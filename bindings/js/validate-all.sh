#!/usr/bin/env bash
# Caller reserves all four CPU slots. All commands remain in the foreground.
set -euo pipefail
cd "$(dirname "$0")/../.."
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
JS_FAMILIES=exp bash bindings/js/build.sh O2 exp-only > build/scratch/js-exp-build.log 2>&1
JS_FAMILIES=gb2 bash bindings/js/build.sh O2 gb2-only > build/scratch/js-gb2-build.log 2>&1
just cpp-test > build/scratch/cpp-final.log 2>&1
NOT_CRAN=true just test > build/scratch/r-final.log 2>&1
just js-build > build/scratch/js-final-build.log 2>&1
just js-test > build/scratch/js-final-test.log 2>&1
node --v8-pool-size=1 bindings/js/measure-all.mjs exp-only gb2-only univariateML > build/scratch/module-layout.log 2>&1
just site-check > build/scratch/site-final.log 2>&1
python3 site/scripts/browser-smoke.py > build/scratch/browser-final.log 2>&1
python3 -c 'import os; os.sched_setaffinity(0,{min(os.sched_getaffinity(0))}); os.execvp("node",["node","--v8-pool-size=1","bindings/js/browser-all.mjs"])'  > build/scratch/browser-all.log 2>&1
