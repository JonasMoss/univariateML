"""Seeded search for multiple conditional profile maxima (numerical evidence)."""
import json
import numpy as np
from scipy.special import digamma,expit
rng=np.random.default_rng(1163207)
b=np.exp(np.linspace(-12,9,180))
found=[]
for j in range(1500):
    a=float(np.exp(rng.uniform(-3,5)))
    x=np.exp(rng.uniform(-14,14,size=int(rng.integers(3,15))))
    lx=np.log(x)
    lo=np.log(b)+lx.min()-np.log(a)
    hi=np.log(b)+lx.max()-np.log(a)
    for _ in range(55):
        m=(lo+hi)/2
        score=(a+b)*expit(lx[None,:]-m[:,None]).mean(axis=1)-a
        lo=np.where(score>0,m,lo);hi=np.where(score>0,hi,m)
    d=digamma(a+b)-digamma(b)-np.logaddexp(0,lx[None,:]-((lo+hi)/2)[:,None]).mean(axis=1)
    roots=np.flatnonzero((d[:-1]>1e-10)&(d[1:]<-1e-10))
    if len(roots)>1:
        found.append(dict(a=a,x=x.tolist(),b_brackets=[[float(b[k]),float(b[k+1])]for k in roots]))
        break
print(json.dumps(dict(seed=1163207,samples_checked=j+1,multiple_maxima=found),indent=2))
