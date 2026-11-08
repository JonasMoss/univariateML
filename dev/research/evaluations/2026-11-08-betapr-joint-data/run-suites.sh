#!/usr/bin/env bash
# Run from the worktree root. Sequential foreground suites, at most four cores.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
results=dev/research/evaluations/2026-11-08-betapr-joint-data/suites.tsv
: > "$results"
run_remote() {
  local label=$1
  shift
  local suite_started=$SECONDS rc=0 machine=simbox
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 "$@" > "build/scratch/$label.log" 2>&1 || rc=$?
  if [ "$rc" -eq 75 ]; then
    machine=workstation
    rc=0
    /tmp/umvl-cpu/run4 "$@" >> "build/scratch/$label.log" 2>&1 || rc=$?
  fi
  printf '%s\t%s\t%s\t%s\n' "$label" "$machine" "$rc" "$((SECONDS-suite_started))" >> "$results"
  if [ "$rc" -ne 0 ]; then tail -60 "build/scratch/$label.log"; return "$rc"; fi
  tail -5 "build/scratch/$label.log"
}
run_remote cpp-dev just cpp-test
run_remote cpp-full env UMVL_TEST_TIER=full just cpp-test
run_remote cpp-no-long-double-simbox just cpp-test-no-long-double
run_remote r-dev just test
run_remote r-full just test-full
