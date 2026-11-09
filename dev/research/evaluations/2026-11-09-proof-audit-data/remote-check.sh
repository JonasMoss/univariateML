#!/usr/bin/env bash
# One foreground high-precision computation; no local slot during remote waits.
set -euo pipefail
mode=${1:?check or boundaries}
case "$mode" in
  check) output=results.json;;
  boundaries) output=boundaries.json;;
  *) exit 2;;
esac
evidence=dev/research/evaluations/2026-11-09-proof-audit-data
log=build/scratch/proof-audit/simbox-$mode.log
start=$(date -u +%Y-%m-%dT%H:%M:%SZ)
set +e
/tmp/umvl-cpu/simbox-run bash -c '
  export PYTHONDONTWRITEBYTECODE=1
  python3 "dev/research/evaluations/2026-11-09-proof-audit-data/$1.py" || exit $?
  printf "\nAUDIT_JSON_START\n"
  cat "dev/research/evaluations/2026-11-09-proof-audit-data/$2"
  printf "\nAUDIT_JSON_END\n"
' bash "$mode" "$output" > "$log" 2>&1
rc=$?
set -e
finish=$(date -u +%Y-%m-%dT%H:%M:%SZ)
printf '%s\t%s\t%s\t%s\t%s\n' simbox "$mode" "$start" "$finish" "$rc" >> "$evidence/computations.tsv"
if [ "$rc" -eq 75 ]; then
  /tmp/umvl-cpu/run4 env PYTHONDONTWRITEBYTECODE=1 python3 "$evidence/$mode.py" > "build/scratch/proof-audit/workstation-$mode.log" 2>&1
  printf 'workstation-fallback\t%s\t%s\t%s\t0\n' "$mode" "$finish" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$evidence/computations.tsv"
  tail -3 "build/scratch/proof-audit/workstation-$mode.log"
elif [ "$rc" -eq 0 ]; then
  python3 - "$log" "$evidence/$output" <<'PY'
import json,sys
from pathlib import Path
raw=Path(sys.argv[1]).read_text().split('AUDIT_JSON_START\n',1)[1].split('\nAUDIT_JSON_END',1)[0]
result=json.loads(raw)
assert result['result']=='PASS'
Path(sys.argv[2]).write_text(raw)
print({k:result[k] for k in ['result','decimal_digits','digits','total_checks','assertions'] if k in result})
PY
else
  tail -30 "$log"
  exit "$rc"
fi
