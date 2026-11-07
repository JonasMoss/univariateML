"""Exploratory LP: largest profile curvature at a stationary size.
Equality constraints impose normalization, mean and the shape score.
"""
import numpy as np,json,collections
from scipy.optimize import brentq,linprog
from pathlib import Path
results=[]; feasible=0;calls=0;best=None;statuses=collections.Counter()
x=np.arange(1,1001);j=np.arange(1,1000)
for m in [1.001,1.01,1.05,1.1,1.25,1.5,2,3,5,10,20,50,100]:
 for r in np.logspace(-4,5,100):
  t=brentq(lambda t:r*np.expm1(t)/(-np.expm1(-r*t))-m,1e-15,np.log1p(m/r)+1,xtol=1e-15)
  # Stable enough for exploration; candidates require high precision replay.
  E=lambda z:1/np.expm1(z)-1/z
  J=1-t/np.expm1(t); Jp=(np.exp(t)*(t-1)+1)/np.expm1(t)**2
  tp=t*E(r*t)/(1+E(t)-r*E(r*t))
  H=np.r_[0,np.cumsum(j/(r+j))];H2=np.r_[0,np.cumsum(j/(r+j)**2)]
  res=linprog(-H2*r*r,A_eq=[np.ones(len(x)),x,H*r],b_eq=[1,m,m*J*r],bounds=(0,None),method='highs',options={'threads':1})
  calls+=1;statuses[str(res.status)]+=1
  if not res.success:continue
  feasible+=1; value=(m*Jp*tp+res.x@H2)*r*r
  row=dict(m=m,r=r,scaled_profile_curvature=value,weights=[(int(v),float(w)) for v,w in zip(x,res.x) if w>1e-9])
  if best is None or value>best['scaled_profile_curvature']:best=row
  if value>1e-7:
   print(row,flush=True);results.append(row)
Path(__file__).with_name('curvature-search.json').write_text(json.dumps(dict(calls=calls,statuses=dict(statuses),feasible=feasible,best=best,candidates=results),indent=2)+'\n')
print('complete',calls,feasible,'candidates',len(results),'best',best,flush=True)
