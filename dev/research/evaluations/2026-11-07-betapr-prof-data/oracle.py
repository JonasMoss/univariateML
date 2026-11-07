"""Independent conditional likelihood checks and search for multiple extrema.
Run through the CPU reservation wrapper. No generated fixtures are rewritten.
"""
import json
import numpy as np
from scipy.optimize import brentq, minimize_scalar
from scipy.special import betaln, digamma

def profile(x,a,b):
    ls=brentq(lambda t: np.mean((a+b)/(1+np.exp(t-np.log(x))))-a,
              np.log(b*np.min(x)/a)-1,np.log(b*np.max(x)/a)+1,xtol=1e-13)
    logs=np.logaddexp(0,np.log(x)-ls).mean()
    return ((a-1)*np.log(x).mean()-a*ls-(a+b)*logs-betaln(a,b),
            digamma(a+b)-digamma(b)-logs, np.exp(ls))

x=np.array([.03,.1,.4,.7,1,2,4,12])
checks=[]
for a in [.7,2.,5.]:
    r=minimize_scalar(lambda t:-profile(x,a,np.exp(t))[0],bounds=(-8,12),method='bounded',options={'xatol':1e-12})
    b=np.exp(r.x);ll,score,s=profile(x,a,b)
    checks.append(dict(a=a,b=b,scale=s,logLik=len(x)*ll,score=score))
# Deterministic stress samples; do not interpret this finite scan as uniqueness.
rng=np.random.default_rng(11632)
multiple=[]
for j in range(120):
    a=float(np.exp(rng.uniform(-2,3)))
    xx=np.exp(rng.uniform(-8,8,size=6))
    grid=np.linspace(-10,12,180)
    scores=np.array([profile(xx,a,np.exp(t))[1] for t in grid])
    maxima=np.flatnonzero((scores[:-1]>0)&(scores[1:]<0))
    if len(maxima)>1:
        multiple.append(dict(a=a,x=xx.tolist(),brackets=grid[maxima].tolist()))
print(json.dumps(dict(checks=checks,searched_samples=120,multiple_maxima=multiple),indent=2))
