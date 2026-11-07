#!/usr/bin/env python3
"""Check reported failure accounting and the largest-sample coverage gate.

This is a finite pointwise diagnostic: nominal .95 must lie inside each
95% Wilson interval at n=3200, with no failed fits/covariances at that size.
Smaller samples are reported without discarding failures or requiring nominal
Wald coverage. This is not a uniform or misspecification validity theorem.
"""
import json
from pathlib import Path
p=Path(__file__).parent
r=json.loads((p/'calibration.json').read_text());assert len(r['results'])==36
checks=[]
for label,v in r['results'].items():
    assert v['successes']+len(v['failures'])==v['replications']
    if v['n']!=3200:continue
    assert v['successes']==v['replications'],label
    for i,c in enumerate(v['coordinates']):
        for t in ['observed','sandwich']:
            lo,hi=c[t]['wilson95'];assert lo<=.95<=hi,(label,i,t,c[t])
            checks.append([label,i,t])
assert len(checks)==68,len(checks)
(p/'calibration-check.json').write_text(json.dumps(dict(status='pass',largest_n=3200,checks=len(checks),criterion='nominal .95 in each marginal 95% Wilson interval; no largest-n failures'),indent=2)+'\n')
print('PASS',len(checks),'largest-sample coverage intervals; all failures accounted for')
