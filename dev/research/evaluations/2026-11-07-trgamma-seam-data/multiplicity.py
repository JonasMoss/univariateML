"""Numerical search for competing stationary maxima; not a uniqueness proof."""
import json, pathlib
import numpy as np
from scipy import special
rng=np.random.default_rng(11625)
found=[]; counts={'free':0,'held_scale':0}
for rep in range(1200):
    n=int(rng.integers(6,40))
    centers=rng.normal(0,2,3);widths=np.exp(rng.uniform(-3,1,3))
    groups=rng.integers(0,3,n)
    y=centers[groups]+widths[groups]*rng.normal(size=n)
    u=(y-y.mean())/y.std();M=max(u)
    t=np.geomspace(.001,4*(n+np.log(n)+10)/M,350)
    z=t[:,None]*u;weights=np.exp(z-z.max(axis=1)[:,None]);K=z.max(axis=1)+np.log(weights.mean(axis=1));K1=(weights*u).sum(axis=1)/weights.sum(axis=1)
    a=(3-K+np.sqrt((K-3)**2+24*K))/(12*K)
    for _ in range(25):
        r=1/a
        f=np.where(a>100,.5*r+r*r*(1/12-r*r/120),np.log(a)-special.digamma(a))-K
        df=np.where(a>100,-r*r*(.5+r/6-r**3/30),r-special.polygamma(1,a))
        a*=np.exp(-np.clip(f/(a*df),-2,2))
    slope=1-a*t*K1
    roots=np.where((slope[:-1]>0)&(slope[1:]<0))[0]
    counts['free']+=1
    if len(roots)>1:found.append(dict(rep=rep,kind='free',logs=y.tolist(),brackets=[[float(t[j]),float(t[j+1])] for j in roots]))
    w=y/max(abs(y));m=w.mean();M=max(w)
    q=np.geomspace(.001,min(10000,500/M) if M>0 else 10000,350);target=q*m
    k=np.exp(np.maximum(target,-2))+.5
    k[target < -2]=-1/(target[target < -2]+np.euler_gamma)
    for _ in range(25):
        step=(special.digamma(k)-target)/(k*special.polygamma(1,k));k*=np.exp(-np.clip(step,-2,2))
    ex=np.exp(q[:,None]*w)
    slope=1+k*q*m-q*(ex*w).mean(axis=1)
    roots=np.where((slope[:-1]>0)&(slope[1:]<0))[0]
    counts['held_scale']+=1
    if len(roots)>1:found.append(dict(rep=rep,kind='held_scale',logs=y.tolist(),brackets=[[float(q[j]),float(q[j+1])] for j in roots]))
result=dict(seed=11625,counts=counts,candidates=found)
pathlib.Path(__file__).with_name('multiplicity.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result))
