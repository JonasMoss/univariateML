#!/usr/bin/env python3
import csv,json,math,statistics
from collections import Counter,defaultdict
from pathlib import Path
folder=Path(__file__).resolve().parent
rows=list(csv.DictReader((folder/'seeded-coverage.csv').open()))
inputs=list(csv.DictReader((folder/'seeded-samples.csv').open()))
assert len(rows)==1344 and len(inputs)==448
samples={(r['family'],r['source'],r['seed']):[float(r['x'+str(i)]) for i in range(64)] for r in inputs}
groups=defaultdict(list)
positive_normal=[]
for r in rows:
    groups[(r['family'],r['source'],int(r['held_mask']))].append(r)
    assert int(r['certified'])==int(r['category'].startswith('certified_'))
    if r['category'].endswith('_finite'):
        assert all(math.isfinite(float(r[k])) for k in ('mu','physical_scale','shape','loglik'))
        assert float(r['physical_scale'])>0 and float(r['shape'])>(2 if r['family']=='std' else 0)
    if r['category']=='uncertified_face' and int(r['status'])==(3 if r['family']=='std' else 4):
        x=samples[(r['family'],r['source'],r['seed'])];mask=int(r['held_mask'])
        m=0 if mask&1 else statistics.mean(x)
        s=1 if mask&2 else math.sqrt(math.fsum((z-m)**2 for z in x)/len(x))
        q=[((z-m)/s)**2 for z in x]
        sd=r['family']=='std' and mask&2
        c1=math.fsum(z*z-(6 if sd else 2)*z+(3 if sd else -1) for z in q)/4
        if c1>1e-8:positive_normal.append(dict(family=r['family'],source=r['source'],seed=r['seed'],mask=mask,c1=c1))
summary=[]
for key,values in sorted(groups.items()):
    assert len(values)==32
    counts=Counter(r['category'] for r in values)
    seconds=sorted(float(r['seconds']) for r in values)
    F=counts['certified_finite'];L=counts['certified_face'];U=counts['uncertified_finite']+counts['uncertified_face'];S=counts['stop']
    assert F+L+U+S==32
    summary.append(dict(family=key[0],source=key[1],held_mask=key[2],n=64,trials=32,
        counts=dict(counts),fractions=dict(certified_finite=F/32,certified_face=L/32,uncertified=U/32,stop=S/32),
        median_seconds=statistics.median(seconds),p95_seconds=seconds[math.ceil(.95*len(seconds))-1],max_seconds=max(seconds),
        stop_status_counts=dict(Counter(r['status'] for r in values if r['category']=='stop'))))
print(json.dumps(dict(machine='workstation',compiler='GCC 15.2.0',flags='-O2 -std=c++17',tol=1e-10,maxit=200,
    generator='mt19937_64(1164500+1000*source_index+seed); open 52-bit uniforms; qnorm/qlst or analytic bounded transforms',
    sources=['normal','t3','t5','t10','t30','uniform','triangular'],
    holds='mask 1: location0; mask2: physical scale1; mask3: both. Std t data standardized to variance1; LST t data have ordinary scale1.',
    fits=len(rows),input_samples=len(inputs),groups=summary,positive_first_order_uncertified_normal=positive_normal),indent=2))
