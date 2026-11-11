#!/bin/bash
# Invoke directly: each computing command takes its own slots.
set -u
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch/final-runs
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
: > "$h/local-final-runs.tsv"
failed=0
for recipe in cpp-test cpp-test-no-long-double js-build js-test site-check relations; do
 wrapper=/tmp/umvl-cpu/run4
 case "$recipe" in js-test|site-check|relations) wrapper=/tmp/umvl-cpu/run;; esac
 "$wrapper" env EMCC_CORES=4 just "$recipe" > "build/scratch/final-runs/$recipe.log" 2>&1
 code=$?
 printf '%s\tworkstation\t%s\n' "$recipe" "$code" | tee -a "$h/local-final-runs.tsv"
 tail -18 "build/scratch/final-runs/$recipe.log"
 if [ "$code" -ne 0 ]; then failed=1; fi
done
exit "$failed"
