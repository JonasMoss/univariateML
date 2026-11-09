#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
out=dev/research/evaluations/2026-11-08-st-cert-data
/tmp/umvl-cpu/run4 just cpp-test-no-long-double > "$out/cpp-no-long-double-workstation.txt.gz" 2>&1
bash "$out/js-build-one.sh" > "$out/js-build.txt" 2>&1
/tmp/umvl-cpu/run just js-test > "$out/js-test.txt" 2>&1
/tmp/umvl-cpu/run just site-check > "$out/site-check.txt" 2>&1
