"""Independent 80-digit scalar roots; uses original gamma log likelihood."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps = 80
x = [0,0,1,2,3,6,12,24]
def score(p,j):
    r,a,b=p
    if j==0:
        return sum(mp.digamma(r+v)-mp.digamma(r)+mp.digamma(a+r)-mp.digamma(a+r+b+v) for v in x)
    if j==1:
        return sum(mp.digamma(a+r)+mp.digamma(a+b)-mp.digamma(a)-mp.digamma(a+r+b+v) for v in x)
    return sum(mp.digamma(b+v)-mp.digamma(b)+mp.digamma(a+b)-mp.digamma(a+r+b+v) for v in x)
def likelihood(p):
    r,a,b=p
    return sum(mp.loggamma(r+v)-mp.loggamma(r)-mp.loggamma(v+1)+mp.loggamma(a+r)+mp.loggamma(b+v)-mp.loggamma(a+r+b+v)-mp.loggamma(a)-mp.loggamma(b)+mp.loggamma(a+b) for v in x)
rows=[]
for j in range(3):
    p=list(map(mp.mpf,['0.37','1.8','4.6']))
    lo,hi=mp.mpf(-20),mp.mpf(20)
    p[j]=mp.exp(lo); assert score(p,j)>0
    p[j]=mp.exp(hi); assert score(p,j)<0
    for _ in range(400):
        mid=(lo+hi)/2
        p[j]=mp.exp(mid)
        if score(p,j)>0:lo=mid
        else:hi=mid
    p[j]=mp.exp((lo+hi)/2)
    assert abs(score(p,j))<mp.mpf("1e-70")
    rows.append(dict(free=j,coef=[str(v) for v in p],logLik=str(likelihood(p))))
print(json.dumps(rows,indent=2))
