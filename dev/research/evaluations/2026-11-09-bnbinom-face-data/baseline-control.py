"""Same-compiler default-fit and timing control against the exact base header."""
import csv
import json
import math
import statistics
from collections import Counter
from pathlib import Path
p=Path(__file__).parent
before={int(r['id']):r for r in csv.DictReader((p/'baseline-workstation.csv').open())}
after={int(r['id']):r for r in csv.DictReader((p/'coverage.csv').open())}
assert set(before)==set(after)==set(range(270))
changed=[];drift=[];lost=[];promoted=[]
for i in range(270):
    b=before[i];a=after[i]
    if a['status']!=b['status']:changed.append(i)
    if b['certified']=='1' and a['certified']=='0':lost.append(i)
    if b['certified']=='0' and a['certified']=='1':promoted.append(i)
    for k in ['fit_size','fit_alpha','fit_beta','loglik']:
        x=float(b[k]);y=float(a[k])
        if math.isfinite(x) and math.isfinite(y) and x!=y:
            drift.append(dict(id=i,field=k,before=x,after=y,relative=abs(x-y)/max(abs(x),abs(y),1e-300)))
assert not changed,changed
assert not lost,lost

def summary(rows):
    t=[float(r['fit_ms']) for r in rows]
    return dict(counts=dict(Counter(r['category'] for r in rows)),mean_ms=statistics.mean(t),
                median_ms=statistics.median(t),max_ms=max(t),total_seconds=sum(t)/1000)
result=dict(compiler='GCC 15.2.0',machine='workstation',samples=270,status_changes=changed,
            certificate_downgrades=lost,promoted_ids=promoted,fit_differences=drift,
            all_finite_coefficients_and_likelihoods_identical=not drift,
            before=summary(before.values()),after=summary(after.values()))
(p/'baseline-control.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
