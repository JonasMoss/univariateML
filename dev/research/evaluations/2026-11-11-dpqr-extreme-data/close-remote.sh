#!/bin/bash
set -eu
h=dev/research/evaluations/2026-11-11-dpqr-extreme-data
rc=0
TRUTH_EXTREME_DIGITS=160 TRUTH_EXTREME_TIMEOUT=120 python3 "$h/generate.py" --one fatigue || rc=$?
test "$rc" -le 1
python3 "$h/finalize-metadata.py"
python3 "$h/audit.py" build current
python3 "$h/audit.py" build current no-long-double
python3 "$h/audit.py" check after
python3 "$h/audit.py" check after-double
python3 "$h/fatigue-underflow.py"
python3 "$h/power-smoke.py"
python3 "$h/seam.py" final
python3 - <<'PY'
import json
from pathlib import Path
h=Path('dev/research/evaluations/2026-11-11-dpqr-extreme-data')
assert json.loads((h/'after-summary.json').read_text())['active_failures']==0
assert json.loads((h/'after-double-summary.json').read_text())['active_failures']==0
s=json.loads((h/'seam-final.json').read_text())
assert not any(r['attribution']=='open reference' for r in s['mismatches'])
for stem in ('after','after-double','seam-final'):
 for p in h.glob(stem+'*.json'):
  if not p.name.startswith('gcc13-'):(h/('gcc13-'+p.name)).write_text(p.read_text())
PY
python3 "$h/coverage.py"
mkdir -p build/scratch/remote-final
: > "$h/remote-final-runs.tsv"
failed=0
for recipe in cpp-test-no-long-double test test-full; do
  code=0
  just "$recipe" > "build/scratch/remote-final/$recipe.log" 2>&1 || code=$?
  printf '%s\tsimbox\t%s\n' "$recipe" "$code" | tee -a "$h/remote-final-runs.tsv"
  tail -18 "build/scratch/remote-final/$recipe.log"
  if [ "$code" -ne 0 ]; then failed=1; fi
done
exit "$failed"
