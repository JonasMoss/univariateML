#!/usr/bin/env bash
# Invoke from this worktree through /tmp/umvl-cpu/run4 (four slots).
set -euo pipefail
unset UMVL_TEST_FAMILIES FIT_FAMILY JS_FAMILIES UMVL_TEST_TIER UMVL_TEST_NO_LONG_DOUBLE
export EMCC_CORES=4
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
D=dev/research/evaluations/2026-11-14-core5b-b-data
suites=("$@")
if [ ${#suites[@]} -eq 0 ]; then suites=(cpp-test cpp-test-no-long-double test test-full js-build js-test site-check); : > "$D/suite-results.tsv"; fi
for suite in "${suites[@]}"; do
  date -Is > "$D/$suite.log"
  if just "$suite" >> "$D/$suite.log" 2>&1; then
    echo "$suite PASS"
    printf '%s\tPASS\tworkstation\t%s\n' "$suite" "$(date -Is)" >> "$D/suite-results.tsv"
  else
    echo "$suite FAIL"
    printf '%s\tFAIL\tworkstation\t%s\n' "$suite" "$(date -Is)" >> "$D/suite-results.tsv"
    tail -60 "$D/$suite.log"
    exit 1
  fi
  date -Is >> "$D/$suite.log"
done
