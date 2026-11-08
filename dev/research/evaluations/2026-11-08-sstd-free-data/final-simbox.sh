#!/bin/bash
set -euo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
D=dev/research/evaluations/2026-11-08-sstd-free-data
run(){ echo "START $1"; local label=$1; shift; if "$@" > "$D/$label.txt" 2>&1; then tail -20 "$D/$label.txt"; echo "PASS $label"; else cat "$D/$label.txt"; return 1; fi; }
run cpp-dev just cpp-test
run cpp-full env UMVL_TEST_TIER=full just cpp-test
run cpp-no-long-double just cpp-test-no-long-double
run r-dev just test
run r-full just test-full
