#!/bin/bash
set -u
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
failed=0
for recipe in cpp-test; do
 /tmp/umvl-cpu/run4 just "$recipe" > "build/scratch/final-runs/$recipe.log" 2>&1
 code=$?
 printf "%s\tworkstation\t%s\n" "$recipe" "$code" > "$h/native-final.tsv"
 printf '%s\tworkstation\t%s\n' "$recipe" "$code"
 tail -18 "build/scratch/final-runs/$recipe.log"
 if [ "$code" -ne 0 ];then failed=1;fi
done
exit "$failed"
