#!/bin/bash
# Run directly; each computing command reserves its own slots.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
out=dev/research/evaluations/2026-11-09-gev-perf-data
local_four() {
  local label=$1;shift
  local start=$SECONDS
  /tmp/umvl-cpu/run4 "$@" > "$out/$label.log" 2>&1
  printf '%s\tworkstation-fallback\t%s\n' "$label" "$((SECONDS-start))" >> "$out/suites.tsv"
  gzip -f "$out/$label.log"
}
local_one() {
  local label=$1;shift
  local start=$SECONDS
  /tmp/umvl-cpu/run "$@" > "$out/$label.log" 2>&1
  printf '%s\tworkstation\t%s\n' "$label" "$((SECONDS-start))" >> "$out/suites.tsv"
  gzip -f "$out/$label.log"
}
local_four r-dev just test
local_four r-full just test-full
local_one js-build env EMCC_CORES=1 just js-build
local_one js-test just js-test
local_one site-check env EMCC_CORES=1 just site-check
