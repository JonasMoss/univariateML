#!/bin/bash
set -u
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
out=dev/research/evaluations/2026-11-12-bnb-loglik-data
: > "$out/suites.tsv"
for target in cpp-test test test-full cpp-test-no-long-double; do
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just "$target" > "$out/$target.log" 2>&1
  rc=$?; machine=simbox
  if [ "$rc" -eq 75 ]; then
    /tmp/umvl-cpu/run4 just "$target" >> "$out/$target.log" 2>&1
    rc=$?; machine=workstation
  fi
  printf '%s\t%s\t%s\n' "$target" "$machine" "$rc" >> "$out/suites.tsv"
  if [ "$rc" -ne 0 ]; then exit "$rc"; fi
done
/tmp/umvl-cpu/run4 just cpp-test-no-long-double > "$out/cpp-test-no-long-double-workstation.log" 2>&1
rc=$?
printf 'cpp-test-no-long-double-workstation\tworkstation\t%s\n' "$rc" >> "$out/suites.tsv"
exit "$rc"
