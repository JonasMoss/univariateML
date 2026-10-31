#!/usr/bin/env bash
# Complete foreground reproduction, one shared reservation per operation.
# Run from the repository root. Never wrap this controller in run/run4.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
stage_log=build/scratch/inference-5-final-stages.log
: > "$stage_log"
run_stage() {
  local stage="$1"; shift
  printf '%s START %s\n' "$(date -Iseconds)" "$stage" | tee -a "$stage_log"
  /tmp/umvl-cpu/run4 "$@" > "build/scratch/inference-5-${stage}.log" 2>&1
  printf '%s PASS %s\n' "$(date -Iseconds)" "$stage" | tee -a "$stage_log"
}
run_stage cpp-dev just cpp-test
run_stage cpp-no-long-double just cpp-test-no-long-double
run_stage calibration bash dev/validation/inference-5.sh --reps 5000 --reference-reps 10000
run_stage r-dev just test
run_stage cpp-full env UMVL_TEST_TIER=full just cpp-test
run_stage r-full just test-full
/tmp/umvl-cpu/run python3 dev/validation/inference-5-report.py
