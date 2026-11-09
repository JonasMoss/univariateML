#!/bin/bash
# Run directly: each computing command takes its prescribed slots.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
out=dev/research/evaluations/2026-11-09-gev-perf-data
: > "$out/suites.tsv"
local_run() {
  local label=$1;shift
  local start=$SECONDS
  /tmp/umvl-cpu/run4 "$@" > "$out/$label.log" 2>&1
  printf '%s\tworkstation\t%s\n' "$label" "$((SECONDS-start))" >> "$out/suites.tsv"
  gzip -f "$out/$label.log"
}
remote_run() {
  local label=$1;shift
  local rc=0 machine=simbox start=$SECONDS
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 "$@" > "$out/$label.log" 2>&1 || rc=$?
  if [[ "$rc" == 75 ]];then
    machine=workstation-fallback
    /tmp/umvl-cpu/run4 "$@" >> "$out/$label.log" 2>&1
  elif [[ "$rc" != 0 ]];then cat "$out/$label.log";return "$rc";fi
  printf '%s\t%s\t%s\n' "$label" "$machine" "$((SECONDS-start))" >> "$out/suites.tsv"
  gzip -f "$out/$label.log"
}
local_run cpp-dev just cpp-test
remote_run cpp-full env UMVL_TEST_TIER=full just cpp-test
local_run cpp-no-long-double just cpp-test-no-long-double
remote_run cpp-no-long-double-simbox just cpp-test-no-long-double
remote_run r-dev just test
remote_run r-full just test-full
# Family-header performance changes reach WASM as well; exercise that binding.
start=$SECONDS
/tmp/umvl-cpu/run env EMCC_CORES=1 just js-build > "$out/js-build.log" 2>&1
printf 'js-build\tworkstation\t%s\n' "$((SECONDS-start))" >> "$out/suites.tsv"
gzip -f "$out/js-build.log"
start=$SECONDS
/tmp/umvl-cpu/run just js-test > "$out/js-test.log" 2>&1
printf 'js-test\tworkstation\t%s\n' "$((SECONDS-start))" >> "$out/suites.tsv"
gzip -f "$out/js-test.log"
start=$SECONDS
/tmp/umvl-cpu/run env EMCC_CORES=1 just site-check > "$out/site-check.log" 2>&1
printf 'site-check\tworkstation\t%s\n' "$((SECONDS-start))" >> "$out/suites.tsv"
gzip -f "$out/site-check.log"
