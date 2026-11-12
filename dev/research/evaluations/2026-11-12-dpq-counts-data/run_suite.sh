#!/usr/bin/env bash
set -uo pipefail
root=$(pwd)
export TMPDIR="$root/build/tmp" JUST_TEMPDIR="$root/build/tmp" XDG_RUNTIME_DIR="$root/build/tmp"
mkdir -p "$TMPDIR" "$root/build/scratch"
name=$1; shift
log="dev/research/evaluations/2026-11-12-dpq-counts-data/$name.log"
if [[ $name == local-* ]]; then
  machine=workstation
  /tmp/umvl-cpu/run4 "$@" >"$log" 2>&1
  rc=$?
else
  machine=simbox
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 "$@" >"$log" 2>&1
  rc=$?
  if [[ $rc == 75 ]]; then
    machine=workstation-fallback
    /tmp/umvl-cpu/run4 "$@" >>"$log" 2>&1
    rc=$?
  fi
fi
printf '%s\t%s\t%s\n' "$name" "$machine" "$rc" >> dev/research/evaluations/2026-11-12-dpq-counts-data/suites.tsv
 tail -n 8 "$log"
exit "$rc"
