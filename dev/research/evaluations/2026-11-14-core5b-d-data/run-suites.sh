#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
D=dev/research/evaluations/2026-11-14-core5b-d-data
: > "$D/verified-suite-status.txt"
for recipe in cpp-test cpp-test-no-long-double test test-full js-build js-test site-check; do
  echo "Starting final $recipe (workstation)"
  if /tmp/umvl-cpu/run4 just "$recipe" > "$D/verified-$recipe.log" 2>&1; then
    echo "$recipe 0 workstation" | tee -a "$D/verified-suite-status.txt"
  else
    result=$?;echo "$recipe $result workstation" | tee -a "$D/verified-suite-status.txt";tail -60 "$D/verified-$recipe.log";exit "$result"
  fi
done
