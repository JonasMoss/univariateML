#!/bin/bash
set -euo pipefail
D=dev/research/evaluations/2026-11-08-sstd-free-data
run(){ echo "START $1"; local label=$1; shift; /tmp/umvl-cpu/run4 "$@" > "$D/$label.txt" 2>&1; echo "PASS $label"; }
run cpp-no-long-double-local just cpp-test-no-long-double
run js-build just js-build
run js-test just js-test
run site-check just site-check
