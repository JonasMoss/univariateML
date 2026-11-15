#!/usr/bin/env bash
# Invoke through /tmp/umvl-cpu/run after both snapshots finish.
set -euo pipefail
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
D=dev/research/evaluations/2026-11-14-core5b-b-data
B=dev/research/evaluations/2026-11-14-fit-snapshot-data
F=beta,bnbinom,cmb,dlaplace,dweibull,fatigue,frechet,genbeta,gompertz,hcauchy,huber,invexp,invparalogis,invweibull,llogis,logitnorm,naka,paralogis,pareto3,power,sgomp,sn,st,trgamma,weibull
for mode in native double; do
  python3 dev/validation/fit-snapshot.py --diff "$B/$mode.jsonl.gz" "build/snapshots/$mode" --family "$F" > "$D/$mode-diff.log" 2>&1 || [ $? -eq 1 ]
  python3 dev/validation/fit-snapshot.py --outcomes "build/snapshots/$mode" > "$D/$mode-outcomes.log" 2>&1
done
python3 "$D/classify-diffs.py" > "$D/difference-counts.log" 2>&1
python3 "$D/reference-points.py" > "$D/reference-points.log" 2>&1
cp dev/validation/strict-known.json build/scratch/strict-known-prune.json
for judge in fit-judge fit-density fit-consistency fit-refine; do
  options=()
  [ "$judge" != fit-density ] || options+=(--quiet --records "$D/density-records.jsonl")
  [ "$judge" != fit-refine ] || options+=(--cache-only)
  python3 "dev/validation/$judge.py" build/snapshots/native build/snapshots/double "${options[@]}" --known build/scratch/strict-known-prune.json --prune > "$D/$judge.log" 2>&1
done
python3 dev/validation/strict-seam.py native double --known build/scratch/strict-known-prune.json --prune > "$D/strict-seam.log" 2>&1
# Keep pruning within this batch and leave the accuracy/precision cards listed.
python3 - <<'PY'
import json
from pathlib import Path
p=Path('dev/validation/strict-known.json');original=json.loads(p.read_text())
pruned=json.loads(Path('build/scratch/strict-known-prune.json').read_text())
families=set('beta bnbinom cmb dlaplace dweibull fatigue frechet genbeta gompertz hcauchy huber invexp invparalogis invweibull llogis logitnorm naka paralogis pareto3 power sgomp sn st trgamma weibull'.split())
removed={k:v for k,v in original['known'].items() if k not in pruned['known'] and k.split('|')[2] in families and v['ref'] not in {'TASK-214.5','TASK-214.13'}}
for k in removed:del original['known'][k]
p.write_text(json.dumps(original,indent=1)+'\n')
baseline=json.loads(Path('dev/research/evaluations/2026-11-14-core5b-b-data/baseline-known.json').read_text())
fixed={k:v for k,v in baseline.items() if k not in original['known'] and v['ref'] not in {'TASK-214.5','TASK-214.13'}}
Path('dev/research/evaluations/2026-11-14-core5b-b-data/pruned-known.json').write_text(json.dumps(fixed,indent=2)+'\n')
print('Pruned',len(removed),'further scoped known entries;',len(fixed),'original entries now fixed.')
PY
just strict build/snapshots/native build/snapshots/double > "$D/strict.log" 2>&1
