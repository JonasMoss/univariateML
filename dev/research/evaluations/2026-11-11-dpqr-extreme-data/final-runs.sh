#!/bin/bash
set -u
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/final-runs
receipts=dev/research/evaluations/2026-11-11-dpqr-extreme-data/final-runs.tsv
: > "$receipts"
failed=0
run_local() {
  name=$1; shift
  wrapper=/tmp/umvl-cpu/run4
  case "$name" in js-test|site-check|relations) wrapper=/tmp/umvl-cpu/run;; esac
  "$wrapper" "$@" > "build/scratch/final-runs/$name.log" 2>&1
  rc=$?
  printf '%s\tworkstation\t%s\n' "$name" "$rc" | tee -a "$receipts"
  if [ "$rc" -ne 0 ]; then failed=1; tail -60 "build/scratch/final-runs/$name.log"; fi
}
run_remote() {
  name=$1; shift
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 "$@" > "build/scratch/final-runs/$name.log" 2>&1
  rc=$?
  printf '%s\tsimbox\t%s\n' "$name" "$rc" | tee -a "$receipts"
  if [ "$rc" -eq 75 ]; then run_local "$name-fallback" "$@";
  elif [ "$rc" -ne 0 ]; then failed=1; tail -60 "build/scratch/final-runs/$name.log"; fi
}
run_local cpp-test just cpp-test
run_local cpp-no-long-double just cpp-test-no-long-double
run_remote cpp-no-long-double-simbox just cpp-test-no-long-double
run_remote r-dev just test
run_remote r-full just test-full
run_local js-build just js-build
run_local js-test just js-test
run_local site-check just site-check
run_local relations just relations
exit "$failed"
