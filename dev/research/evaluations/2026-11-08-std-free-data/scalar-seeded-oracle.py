"""Independent 80-digit stationary roots on exact binary64 seeded inputs."""
import csv,json,mpmath as mp
from pathlib import Path
mp.mp.dps=80
D=Path(__file__).resolve().parent
samples=list(csv.DictReader((D/'seeded-samples.csv').open()))
fits=list(csv.DictReader((D/'seeded-coverage.csv').open()))
results=[]
for seed in ('10','2'):
    r=next(r for r in samples if r['family']=='lst' and r['source']=='normal' and r['seed']==seed)
    f=next(r for r in fits if r['family']=='lst' and r['source']=='normal' and r['seed']==seed and r['held_mask']=='3')
    q=[mp.mpf(float(r['x'+str(j)]))**2 for j in range(64)];n=len(q)
    def score(v):
        c=(mp.digamma((v+1)/2)-mp.digamma(v/2)-1/v)/2
        return n*c-mp.fsum(mp.log1p(z/v)-z/(v+z) for z in q)/2+mp.fsum(z/(v*(v+z)) for z in q)/2
    nomination=mp.mpf(float(f['shape']))
    v=mp.findroot(score,(nomination*mp.mpf('.99'),nomination*mp.mpf('1.01')),tol=mp.mpf('1e-70'),maxsteps=100)
    assert abs(score(v))<mp.mpf('1e-65') and mp.diff(score,v)<0
    error=abs(nomination/v-1)
    if f['certified']=='1':assert error<=mp.mpf('1e-10')
    results.append(dict(family='lst',source='normal',seed=int(seed),held=['mu','sigma'],shape=str(v),score=str(score(v)),curvature=str(mp.diff(score,v)),reported_shape=f['shape'],certified=int(f['certified']),relative_shape_error=str(error)))
print(json.dumps(dict(precision_dps=80,scope='Numerical stationary-root/accuracy checks; globality comes from the library certificate when closed.',roots=results),indent=2))
