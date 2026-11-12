#!/usr/bin/env python3
"""Deterministic EW profile exploration; searches are witnesses, not certificates."""
import json
import numpy as np
from scipy.optimize import minimize, minimize_scalar
from scipy.special import logsumexp


def profile(z,y):
    v0, lb = z
    b=np.exp(lb)
    v=v0+b*(y-y.min())
    if np.max(v)>7 or np.min(v)<-700:
        return 1e100
    t=np.exp(v)
    h=np.empty_like(t)
    small=t<0.6931471805599453
    h[small]=np.log(-np.expm1(-t[small]))
    h[~small]=np.log1p(-np.exp(-t[~small]))
    # -h is approximately exp(-t); avoid its underflow at large t.
    lh=np.empty_like(t)
    mask=t<35
    lh[mask]=np.log(-h[mask])
    lh[~mask]=-t[~mask]
    logc=np.log(len(y))-logsumexp(lh)
    if logc>700:
        return 1e100
    c=np.exp(logc)
    ch=np.exp(logc+lh)
    val=len(y)*(lb+logc)-sum(y)+sum(v-t-h)-sum(ch)
    return -float(val) if np.isfinite(val) else 1e100


def faces(y):
    n=len(y); top=max(y); d=sum(top-y)
    k=n/d
    power=n*np.log(k)-sum(y)-k*d
    def inverse(lk):
        k=np.exp(lk); z=-k*y
        return -(n*np.log(k)-sum(y)+sum(z)-n*logsumexp(z)+n*np.log(n)-n)
    center=-np.log(np.ptp(y))
    r=minimize_scalar(inverse,bounds=(center-12,center+12),method='bounded',options={'xatol':1e-12})
    return float(power),float(-r.fun),float(np.exp(r.x))

samples={
 'two_point_equal':[0,1], 'two_point_unbalanced':[0,0,0,0,1],
 'near_ties':[0,1e-5,1e-5,1,1], 'wide':[0,.001,2,10,30],
 'small_shape':[0,.1,.4,2,9,20], 'endpoint_ties':[0,.3,1,1,1],
 'near_constant':[0,1e-6,2e-6,3e-6,5e-6],
 'mixed':[0,.2,.35,.7,1,1.8,3]}
out={}
for name,ys in samples.items():
    y=np.array(ys,dtype=float); span=np.ptp(y)
    starts=[(v,np.log(q/span)) for v in [-5,0,2,4] for q in [.1,1,5,20]]
    found=[]
    for start in starts:
        r=minimize(profile,start,args=(y,),method='Nelder-Mead',options={'maxiter':3000,'xatol':1e-10,'fatol':1e-9})
        if r.success and np.isfinite(r.fun) and r.fun<1e90:
            found.append(r)
    found.sort(key=lambda r:r.fun)
    p,i,k=faces(y)
    best=found[0] if found else None
    out[name]={'log_data':ys,'power_loglik':p,'invweibull_loglik':i,'invweibull_shape':k,
       'best_search_loglik':(-float(best.fun) if best else None),
       'best_search_coordinates_vmin_logb':(best.x.tolist() if best else None),
       'successful_starts':len(found),'finite_minus_best_face':(-float(best.fun)-max(p,i) if best else None)}
print(json.dumps({'method':'float64 multistart exploratory profile; not a certificate','cases':out},indent=2,allow_nan=False))
