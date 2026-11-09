"""Compare the frozen 270-sample replay with TASK-116.54."""
import csv
import hashlib
import json
import math
from collections import Counter
from pathlib import Path
here=Path(__file__).parent
old=here.parent/'2026-11-08-bnbinom-allfree-data'
before={int(r['id']):r for r in csv.DictReader((old/'coverage.csv').open())}
after={int(r['id']):r for r in csv.DictReader((here/'coverage.csv').open())}
assert set(before)==set(after)==set(range(270))
expected=[json.loads(s) for s in (old/'coverage-samples.jsonl').read_text().splitlines()]
actual=[json.loads(s) for s in (here/'coverage-samples.jsonl').read_text().splitlines()]
assert {r['id']:r['x'] for r in expected}=={r['id']:r['x'] for r in actual}
promoted=[];status_changes=[];max_relative_coefficient_change=0.;max_loglik_change=0.
for i in range(270):
    b=before[i];a=after[i]
    assert all(b[k]==a[k] for k in ['source','n','seed'])
    assert a['category']!='search_failure'
    if b['status']!=a['status']:status_changes.append(i)
    assert not (b['certified']=='1' and a['certified']=='0'),('lost certificate',i)
    if b['certified']=='0' and a['certified']=='1':
        promoted.append(dict(id=i,source=a['source'],n=int(a['n']),model={'2':'nbinom','3':'pois','4':'point_mass'}[a['status']]))
    for k in ['fit_size','fit_alpha','fit_beta']:
        x=float(b[k]);y=float(a[k])
        if math.isfinite(x) and math.isfinite(y):max_relative_coefficient_change=max(max_relative_coefficient_change,abs(x-y)/max(abs(x),abs(y)))
    x=float(b['loglik']);y=float(a['loglik'])
    if math.isfinite(x) and math.isfinite(y):max_loglik_change=max(max_loglik_change,abs(x-y))
assert not status_changes,status_changes
out=dict(samples_identical=True,status_changes=status_changes,promotions=promoted,
         promoted_by_model=dict(Counter(r['model'] for r in promoted)),
         promoted_by_source=dict(Counter(r['source'] for r in promoted)),
         maximum_relative_finite_coefficient_change=max_relative_coefficient_change,
         maximum_loglik_change=max_loglik_change,
         csv_sha256={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in [old/'coverage.csv',here/'coverage.csv']})
(here/'comparison.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
