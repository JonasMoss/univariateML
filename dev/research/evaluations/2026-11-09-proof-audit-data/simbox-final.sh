#!/usr/bin/env bash
# Do not wrap this launcher in run: remote lock waits must hold no local slot.
set -euo pipefail
evidence=dev/research/evaluations/2026-11-09-proof-audit-data
log=build/scratch/proof-audit/simbox-cpp-double.log
start=$(date -u +%Y-%m-%dT%H:%M:%SZ)
set +e
/tmp/umvl-cpu/simbox-run bash -c '
  c++ -dumpfullversion
  result=0
  bash dev/research/evaluations/2026-11-09-proof-audit-data/suite.sh cpp-double simbox || result=$?
  cat build/scratch/proof-audit/simbox-cpp-double.log
  exit "$result"
' > "$log" 2>&1
rc=$?
set -e
finish=$(date -u +%Y-%m-%dT%H:%M:%SZ)
printf '%s\t%s\t%s\t%s\t%s\n' simbox cpp-double "$start" "$finish" "$rc" >> "$evidence/suites.tsv"
tail -30 "$log"
if [ "$rc" -eq 75 ]; then
  /tmp/umvl-cpu/run4 nice -n 10 bash "$evidence/suite.sh" cpp-double workstation-fallback
else
  exit "$rc"
fi
