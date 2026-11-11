#!/usr/bin/env bash
# Execute in the job worktree. Each command holds slots only while computing.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
D=dev/research/evaluations/2026-11-11-acc-fb-t-data
run_check() {
  name=$1; slots=$2; shift 2
  echo "START $name (workstation)"
  start=$(date +%s)
  if "/tmp/umvl-cpu/$slots" "$@" > "build/scratch/$name.log" 2>&1; then
    elapsed=$(( $(date +%s)-start ))
    echo "$name workstation PASS ${elapsed}s" | tee -a "$D/test-runs.txt"
    tail -n 5 "build/scratch/$name.log"
  else
    rc=$?
    echo "$name workstation FAIL $rc" | tee -a "$D/test-attempts.txt"
    tail -n 70 "build/scratch/$name.log"
    return "$rc"
  fi
}
for name in "$@"; do
 case "$name" in
  cpp-dev) run_check cpp-dev run4 just cpp-test;;
  cpp-full) run_check cpp-full run4 env UMVL_TEST_TIER=full just cpp-test;;
  cpp-double) run_check cpp-double run4 just cpp-test-no-long-double;;
  r-dev) run_check r-dev run4 just test;;
  r-full) run_check r-full run4 just test-full;;
  js-build) run_check js-build run4 just js-build;;
  js-test) run_check js-test run just js-test;;
  site) run_check site run4 just site-check;;
 esac
done
