#!/usr/bin/env bash
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
results=dev/research/evaluations/2026-11-08-betapr-joint-data/local-suites.tsv
: > "$results"
for target in cpp-test-no-long-double js-build js-test site-check; do
  suite_started=$SECONDS
  rc=0
  /tmp/umvl-cpu/run4 just "$target" > "build/scratch/local-$target.log" 2>&1 || rc=$?
  printf '%s\tworkstation\t%s\t%s\n' "$target" "$rc" "$((SECONDS-suite_started))" >> "$results"
  if [ "$target" = site-check ]; then cp build/scratch/site-evidence-before/*.jpg dev/research/evaluations/2026-11-06-site-swap/; fi
  tail -6 "build/scratch/local-$target.log"
  if [ "$rc" -ne 0 ]; then exit "$rc"; fi
done
