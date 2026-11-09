#!/usr/bin/env bash
# Foreground remote run; never reserve local CPU slots while waiting for simbox.
set -euo pipefail
mode=${1:?suite name}
evidence=dev/research/evaluations/2026-11-09-proof-audit-data
log=build/scratch/proof-audit/simbox-$mode.log
mkdir -p build/scratch/proof-audit
start=$(date -u +%Y-%m-%dT%H:%M:%SZ)
set +e
/tmp/umvl-cpu/simbox-run bash -c '
  c++ -dumpfullversion
  R --version | head -1
  result=0
  bash dev/research/evaluations/2026-11-09-proof-audit-data/suite.sh "$1" simbox || result=$?
  cat "build/scratch/proof-audit/simbox-$1.log"
  exit "$result"
' bash "$mode" > "$log" 2>&1
rc=$?
set -e
finish=$(date -u +%Y-%m-%dT%H:%M:%SZ)
printf '%s\t%s\t%s\t%s\t%s\n' simbox "$mode" "$start" "$finish" "$rc" >> "$evidence/suites.tsv"
tail -30 "$log"
if [ "$rc" -eq 75 ]; then
  /tmp/umvl-cpu/run4 nice -n 10 bash "$evidence/suite.sh" "$mode" workstation-fallback
else
  exit "$rc"
fi
