"""Independent numerical comparison, not a global certificate. Run via CPU slot."""
import json
import numpy as np
from scipy.optimize import minimize
from scipy.special import betaln
x0=np.array([.03,.1,.3,.7,1,2,5,20])
rows=[]
for a in [.7,1.,2.]:
 for inverse in [False,True]:
  x=x0**((-1 if inverse else 1)/a)
  def objective(v):
   shape,b=np.exp(v)
   p,q=(shape,2.) if inverse else (2.,shape)
   z=a*(np.log(x)-np.log(b))
   return -(len(x)*(np.log(a)-betaln(p,q))-np.log(x).sum()-p*np.logaddexp(0,-z).sum()-q*np.logaddexp(0,z).sum())
  fits=[minimize(objective,[s,t],method='BFGS',options={'gtol':1e-8}) for s in [-2,0,2] for t in [-2,0,2]]
  fit=min(fits,key=lambda z:z.fun)
  rows.append(dict(power=a,inverse=inverse,shape=float(np.exp(fit.x[0])),scale=float(np.exp(fit.x[1])),logLik=float(-fit.fun)))
print(json.dumps(rows,indent=2))
