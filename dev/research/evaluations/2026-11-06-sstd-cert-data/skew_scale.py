"""Deterministic search for nonmonotone held-mean scale-score branches.
Numerical evidence only; it never supplies an MLE certificate.
Run under /tmp/umvl-cpu/run python3 <this file>.
"""
import json
import math
import random
rng = random.Random(11611)
nu, xi = 3., 10.
m = math.exp(math.lgamma((nu-1)/2)-math.lgamma(nu/2)+.5*math.log((nu-2)/math.pi))
b = m*(xi-1/xi)
a = math.sqrt(1+(1-m*m)*(xi-1/xi)**2)
def score(ls, x):
    result = -len(x)
    for d in x:
        y = a*d*math.exp(-ls)
        z = y+b
        side = xi if z>0 else 1/xi
        result += (nu+1)*y*z/((nu-2)*side*side+z*z)
    return result
result = None
for trial in range(1000):
    x = [rng.choice((-1,1))*math.exp(rng.uniform(-6,6)) for _ in range(6)]
    grid = [-12+i*.04 for i in range(601)]
    values = [score(t,x) for t in grid]
    crossings = [(grid[i],grid[i+1]) for i in range(600) if values[i]*values[i+1]<0]
    if len(crossings)>=3:
        roots = []
        for lo,hi in crossings:
            left_sign = score(lo,x)>0
            for _ in range(60):
                mid = (lo+hi)/2
                if (score(mid,x)>0)==left_sign: lo=mid
                else: hi=mid
            roots.append({'log_sd':(lo+hi)/2,'type':'maximum' if left_sign else 'minimum'})
        normalizer=math.lgamma((nu+1)/2)-math.lgamma(nu/2)-.5*math.log((nu-2)*math.pi)
        for root in roots:
            ls=root['log_sd']
            ll=len(x)*(math.log(a)-ls+math.log(2/(xi+1/xi))+normalizer)
            for d in x:
                z=a*d*math.exp(-ls)+b
                side=xi if z>0 else 1/xi
                ll-=(nu+1)/2*math.log1p((z/side)**2/(nu-2))
            root['logLik']=ll
        result = dict(seed=11611,trial=trial,mean=0,nu=nu,xi=xi,data=x,stationary_points=roots)
        break
print(json.dumps(result if result else {'seed':11611,'trials':1000,'found':False},indent=2))
