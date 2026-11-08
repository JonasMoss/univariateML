#!/bin/bash
set -uo pipefail
export CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4
D=dev/research/evaluations/2026-11-08-sstd-free-data
sha256sum -c "$D/source.sha256" || exit 1
: > "$D/simbox-status.tsv"
failed=0
run(){
 local label=$1; shift
 echo "START $label"
 "$@" > "$D/frozen-$label.txt" 2>&1
 local rc=$?
 printf '%s\t%s\n' "$label" "$rc" >> "$D/simbox-status.tsv"
 if [ "$rc" -eq 0 ]; then tail -20 "$D/frozen-$label.txt"; echo "PASS $label"; else cat "$D/frozen-$label.txt"; failed=1; fi
}
run cpp-dev just cpp-test
run cpp-full env UMVL_TEST_TIER=full just cpp-test
run cpp-no-long-double just cpp-test-no-long-double
run r-dev just test
run r-full just test-full
sha256sum -c "$D/source.sha256" || exit 1
exit "$failed"
