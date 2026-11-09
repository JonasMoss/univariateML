# Numerical falsification of scalar profile concavity, not a global certificate.
import json
import numpy as np
from scipy.special import digamma, polygamma, expit
from scipy.optimize import brentq
rng=np.random.default_rng(11652)
found=[]
for case in range(2000):
 w=rng.normal(rng.uniform(-2,2),rng.uniform(.1,4),8)
 q=10**rng.uniform(-2,2);a=10**rng.uniform(-2,2)
 sm=np.logaddexp(0,-a*w).mean()
 def f(lp):
  p=np.exp(lp)
  return digamma(p+q)-digamma(p)-sm
 if f(-30)*f(30)>=0:continue
 p=np.exp(brentq(f,-30,30))
 u=expit(a*w);weight=u*(1-u)
 pp=8*(polygamma(1,p+q)-polygamma(1,p))
 aa=-8/a**2-(p+q)*sum(w*w*weight)
 ap=sum(w*(1-u))
 prof=aa-ap*ap/pp
 if prof>0 and np.isfinite(prof):
  found.append(dict(log_x_over_b=w.tolist(),q=float(q),a=float(a),inner_p=float(p),profile_curvature=float(prof)))
  if len(found)==3:break
print(json.dumps(found,indent=2))
