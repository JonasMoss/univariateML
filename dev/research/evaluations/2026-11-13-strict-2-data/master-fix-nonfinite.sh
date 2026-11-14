#!/usr/bin/env bash
# Which strict-2 known entries does master 051a3299 (merge of fix-nonfinite) clear or add?
# Rebuilds that commit's seven changed families (cauchy gamma gompertz gumbel logis norm weibull)
# in both modes, puts their records into copies of the c167a975 baselines and judges them.
# Run from the strict-2 worktree root; snapshots need /tmp/umvl-cpu/run4, the checks /tmp/umvl-cpu/run.
set -euo pipefail
E=dev/research/evaluations/2026-11-13-strict-2-data; B=dev/research/evaluations/2026-11-13-fit-snapshot-data
F=cauchy,gamma,gompertz,gumbel,logis,norm,weibull
mkdir -p build/master-src
git archive 051a32991404ee7026875cb7c5eccb9b55e4b8c4 -- univariateML-cpp/include bindings/common dev/validation/fit-snapshot.py \
  dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz dev/research/evaluations/2026-11-13-retire-16-data/route-inputs.json \
  | tar -x -C build/master-src
/tmp/umvl-cpu/run4 nice -n 10 python3 build/master-src/dev/validation/fit-snapshot.py --output "$PWD/build/master-snap/native" --family $F --jobs 4
/tmp/umvl-cpu/run4 nice -n 10 python3 build/master-src/dev/validation/fit-snapshot.py --output "$PWD/build/master-snap/double" --family $F --jobs 4 --no-long-double
python3 - <<'PY'
import gzip, json, shutil
from pathlib import Path
B = Path('dev/research/evaluations/2026-11-13-fit-snapshot-data'); out = Path('build/scratch/master-merged'); out.mkdir(parents=True, exist_ok=True)
fams = {'cauchy', 'gamma', 'gompertz', 'gumbel', 'logis', 'norm', 'weibull'}
for mode in ('native', 'double'):
    new = {}
    for f in fams:
        for l in Path(f'build/master-snap/{mode}/{f}.jsonl').read_text().splitlines()[1:]:
            r = json.loads(l); new[(r['family'], r['source'], r['case'])] = l
    rows = []
    for l in gzip.open(B/f'{mode}.jsonl.gz', 'rt'):
        r = json.loads(l); rows.append(new.get((r['family'], r['source'], r['case']), l.rstrip('\n'))+'\n')
    with gzip.open(out/f'{mode}.jsonl.gz', 'wt') as d: d.writelines(rows)
    shutil.copy(B/f'{mode}.provenance.json', out/f'{mode}.provenance.json')
PY
M=build/scratch/master-merged; N=$M/native.jsonl.gz,$B/routes-native.jsonl.gz; D=$M/double.jsonl.gz,$B/routes-double.jsonl.gz
for s in fit-judge fit-density fit-consistency fit-refine; do
  extra=(); [ $s = fit-density ] && extra=(--quiet); [ $s = fit-refine ] && extra=(--cache-only)
  /tmp/umvl-cpu/run python3 dev/validation/$s.py "${extra[@]}" $N $D || true
done
