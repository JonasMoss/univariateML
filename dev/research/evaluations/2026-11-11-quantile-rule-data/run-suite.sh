#!/usr/bin/env bash
# One foreground suite per invocation; exit 75 alone triggers local fallback.
set -uo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
suite=$1
location=$2
log="build/scratch/quantile-rule-${suite}-${location}.log"
start=$(date -u +%FT%TZ)
machine=workstation
if [[ $location == simbox ]]; then
  machine=simbox
  /tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just "$suite" > "$log" 2>&1
  rc=$?
  if [[ $rc == 75 ]]; then
    machine=workstation-fallback
    /tmp/umvl-cpu/run4 just "$suite" >> "$log" 2>&1
    rc=$?
  fi
else
  /tmp/umvl-cpu/run4 just "$suite" > "$log" 2>&1
  rc=$?
fi
finish=$(date -u +%FT%TZ)
printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$suite" "$machine" "$rc" "$start" "$finish" "$log" >> build/scratch/quantile-rule-runs.tsv
tail -12 "$log"
exit "$rc"
