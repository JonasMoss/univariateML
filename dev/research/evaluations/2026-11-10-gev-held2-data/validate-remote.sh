#!/usr/bin/env bash
set -euo pipefail
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR"
d=dev/research/evaluations/2026-11-10-gev-held2-data
printf 'check,machine,exit\n' > "$d/remote-validation.csv"
for spec in 'r-dev dev test' 'r-full full test-full' 'cpp-dev dev cpp-test' 'cpp-full full cpp-test' 'cpp-double full cpp-test-no-long-double'; do
  read -r name tier recipe <<< "$spec"
  set +e
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 UMVL_TEST_TIER="$tier" just "$recipe" > "$d/final-$name.log" 2>&1
  rc=$?; machine=simbox
  if [ "$rc" = 75 ]; then
    machine=workstation
    /tmp/umvl-cpu/run4 env UMVL_TEST_TIER="$tier" just "$recipe" >> "$d/final-$name.log" 2>&1
    rc=$?
  fi
  set -e
  printf '%s,%s,%s\n' "$name" "$machine" "$rc" >> "$d/remote-validation.csv"
  [ "$rc" = 0 ] || exit "$rc"
done
