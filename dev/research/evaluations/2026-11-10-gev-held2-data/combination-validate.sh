#!/usr/bin/env bash
# Foreground commands; each computing stage owns exactly its required slots.
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
d=dev/research/evaluations/2026-11-10-gev-held2-data
printf 'check,machine,exit\n' > "$d/combination-validation.csv"
for recipe in cpp-test cpp-full cpp-test-no-long-double test test-full js-build js-test site-check quality; do
  args=(just "$recipe")
  if [ "$recipe" = cpp-full ]; then args=(env UMVL_TEST_TIER=full just cpp-test); fi
  if [ "$recipe" = quality ]; then args=(just quality gev frechet); fi
  set +e
  /tmp/umvl-cpu/run4 "${args[@]}" > "$d/combination-$recipe.log" 2>&1
  rc=$?
  set -e
  printf '%s,workstation,%s\n' "$recipe" "$rc" >> "$d/combination-validation.csv"
  [ "$rc" = 0 ] || exit "$rc"
done
