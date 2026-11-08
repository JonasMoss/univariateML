#!/bin/bash
set -uo pipefail
D=dev/research/evaluations/2026-11-08-sstd-free-data
sha256sum -c "$D/source.sha256" || exit 1
: > "$D/local-status.tsv"
failed=0
run(){
 local label=$1; shift
 echo "START $label"
 local logfile="$D/$label.txt"
 if [ "$label" = cpp-no-long-double-local ]; then logfile="$D/frozen-$label.txt"; fi
 /tmp/umvl-cpu/run4 "$@" > "$logfile" 2>&1
 local rc=$?
 printf '%s\t%s\n' "$label" "$rc" >> "$D/local-status.tsv"
 if [ "$rc" -eq 0 ]; then tail -12 "$logfile"; echo "PASS $label"; else tail -60 "$logfile"; failed=1; fi
}
run cpp-no-long-double-local just cpp-test-no-long-double
run js-build just js-build
run js-test just js-test
run site-check just site-check
sha256sum -c "$D/source.sha256" || exit 1
exit "$failed"
