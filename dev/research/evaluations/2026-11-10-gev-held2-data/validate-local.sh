#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR"
d=dev/research/evaluations/2026-11-10-gev-held2-data
printf 'check,machine,exit\n' > "$d/local-validation.csv"
for recipe in cpp-test-no-long-double js-build js-test site-check; do
  set +e
  /tmp/umvl-cpu/run4 just "$recipe" > "$d/final-local-$recipe.log" 2>&1
  rc=$?
  set -e
  printf '%s,workstation,%s\n' "$recipe" "$rc" >> "$d/local-validation.csv"
  [ "$rc" = 0 ] || exit "$rc"
done
# The binding-neutral entry point is also exercised directly by these tests.
/tmp/umvl-cpu/run node --v8-pool-size=1 --test bindings/js/fit-test.mjs > "$d/final-local-js-fit.log" 2>&1
/tmp/umvl-cpu/run just quality gev frechet > "$d/final-local-quality.log" 2>&1
