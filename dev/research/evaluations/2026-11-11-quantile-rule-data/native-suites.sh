#!/usr/bin/env bash
# Invoke through simbox-run, or run4 after simbox-run exits 75.
# Every command below is a parallel build or suite; there are no nested locks.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
machine=${UMVL_VALIDATION_MACHINE:?Set the execution machine label}
: > build/scratch/quantile-rule-native-runs.tsv
for suite in cpp-test cpp-test-no-long-double test test-full; do
  log="build/scratch/quantile-rule-${suite}-native.log"
  start=$(date -u +%FT%TZ)
  rc=0
  just "$suite" > "$log" 2>&1 || rc=$?
  finish=$(date -u +%FT%TZ)
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$suite" "$machine" "$rc" "$start" "$finish" "$log" >> build/scratch/quantile-rule-native-runs.tsv
  printf '%s on %s: exit %s\n' "$suite" "$machine" "$rc"
  tail -12 "$log"
  if [[ $rc != 0 ]]; then exit "$rc"; fi
done
