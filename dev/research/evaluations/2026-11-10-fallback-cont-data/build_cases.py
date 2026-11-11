#!/usr/bin/env python3
"""Reconstruct frozen audit inputs, seed 16802 draws, and route regressions.

Use --check to compare without overwriting cases.json. Does not rebuild the
baseline header archive, which records the pre-edit source.
"""
from pathlib import Path
import gzip
import json
import random
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
families = ['burr', 'invburr', 'pareto4', 'paralogis', 'betapr', 'bhatt', 'ged']
audit = ROOT / 'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'
cases = [dict(c, source='audit') for c in json.loads(gzip.decompress(audit.read_bytes()))
         if c['family'] in families]
rng = random.Random(16802)
for family in families:
    golden = json.loads((ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
                         / family / 'golden.json').read_text())
    for it in [16, 64, 1000]:
        cases.append(dict(family=family, data=golden['data'], held=golden.get('fit_args', {}),
                          index=f'golden-{family}-{it}', iterlim=it, source='seeded'))
    for j in range(3):
        data = [rng.lognormvariate(0, 1.4) for _ in range(25)]
        if family in ['bhatt', 'ged']:
            data = [rng.gauss(0, 1) for _ in range(25)]
        cases.append(dict(family=family, data=data, held={'min': 0} if family == 'pareto4' else {},
                          index=f'seed-{family}-{j}', source='seeded'))
cases += json.loads((HERE / 'route-cases.json').read_text())
if '--check' in sys.argv:
    assert cases == json.loads((HERE / 'cases.json').read_text())
    print(f'{len(cases)} cases reproduced exactly')
else:
    (HERE / 'cases.json').write_text(json.dumps(cases, separators=(',', ':')) + '\n')
