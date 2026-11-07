"""Deterministic exploratory LP and histogram search, not a global certificate."""
import numpy as np
from scipy.optimize import brentq, linprog
from scipy.special import gammaln
import json, collections
from pathlib import Path
OUT=Path(__file__).resolve().parent

def param(r,m):
    t=brentq(lambda t:r*np.expm1(t)/(-np.expm1(-r*t))-m,1e-14,np.log1p(m/r)+1,xtol=1e-14)
    return t

def logmass(r,m,x):
    t=param(r,m)
    # recurrence avoids cancellation of loggammas at huge r
    return np.array([sum(np.log1p(np.arange(int(y))/r))+y*np.log(r*(-np.expm1(-t)))-r*t-gammaln(y+1)-np.log(-np.expm1(-r*t)) for y in x])
def main():
    rows=[]; statuses=collections.Counter(); calls=0; best_gain=None
    for m in [1.1,1.5,2,3,5,10]:
        x=np.arange(1,101); lam=brentq(lambda z:z/(-np.expm1(-z))-m,1e-12,m)
        lp=x*np.log(lam)-lam-gammaln(x+1)-np.log(-np.expm1(-lam))
        for r in np.logspace(-3,4,60):
            diff=logmass(r,m,x)-lp
            res=linprog(-diff,A_ub=[x*(x-1)],b_ub=[m*lam],A_eq=[np.ones(100),x],b_eq=[1,m],bounds=(0,None),method='highs',options={'threads':1})
            statuses[str(res.status)]+=1; calls+=1
            if res.success:best_gain=max(best_gain if best_gain is not None else -np.inf,-res.fun)
            if res.success and -res.fun>1e-9:
                row=dict(m=m,r=r,gain=-res.fun,C=float(res.x@(x*(x-1)/2)-m*lam/2),weights=[(int(i),float(w)) for i,w in zip(x,res.x) if w>1e-9]);rows.append(row)
                print(row,flush=True);break
    (OUT/'explore.json').write_text(json.dumps(dict(calls=calls,statuses=dict(statuses),best_gain=best_gain,candidates=rows),indent=2)+'\n')
    print('LP complete',len(rows),flush=True)

if __name__ == "__main__":
    main()
