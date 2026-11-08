#!/usr/bin/env python3
"""Independent seam exploration, numerical evidence only (80 digits)."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
rows=[]
# The last normalized sample has its gamma fit exactly q=1: its geometric
# mean is exp(-EulerGamma), so log(q)-digamma(q)=-mean(log(v)).
t=mp.sqrt(1-mp.exp(-2*mp.euler))
samples=[[mp.mpf(s) for s in x] for x in [('.01','.1','1','3'),('.2','.8','1','2'),('.5','1.5')]]
samples.append([1-t,1+t])
for y in samples:
 m=sum(y)/len(y);v=[z/m for z in y];g=sum(mp.log(z) for z in v)/len(v)
 qg=mp.exp(mp.findroot(lambda z: z-mp.digamma(mp.exp(z))+g,(mp.mpf(-1),mp.mpf(1))))
 gamma=qg*mp.log(qg)-qg-mp.loggamma(qg)+(qg-1)*g
 coeff=(qg-1)*(sum(z*z for z in v)/len(v)-(qg+1)/qg)/24
 profile=[]
 for h in (mp.mpf('.01'),mp.mpf('.001')):
  def score(lp,lq):
   p,q=mp.exp(lp),mp.exp(lq)
   r=-sum(mp.log(-mp.expm1(-h*z)) for z in v)/len(v)
   return mp.digamma(p+q)-mp.digamma(p)-h,mp.digamma(p+q)-mp.digamma(q)-r
  lp,lq=mp.findroot(score,(mp.log(qg/h),mp.log(qg)),tol=mp.mpf('1e-65'),maxsteps=50)
  p,q=mp.exp(lp),mp.exp(lq);r=-sum(mp.log(-mp.expm1(-h*z)) for z in v)/len(v)
  value=mp.log(h)-p*h-(q-1)*r-mp.log(mp.beta(p,q))
  profile.append(dict(h=str(h),p=str(p),q=str(q),quadratic_ratio=str((value-gamma)/(h*h))))
 rows.append(dict(v=list(map(str,v)),gamma_shape=str(qg),gamma_value=str(gamma),moving_power_value=str(-mp.log(1-min(v))-1),quadratic_coefficient=str(coeff),profile=profile))
quartic=mp.exp(-4*mp.euler)/(1152*(mp.pi**2/6-1))
assert abs(mp.mpf(rows[-1]['quadratic_coefficient']))<mp.mpf('1e-60')
ratio=mp.mpf(rows[-1]['profile'][-1]['quadratic_ratio'])/mp.mpf('.001')**2
assert abs(ratio/quartic-1)<mp.mpf('1e-5')
rows[-1]['proved_quartic_coefficient']=str(quartic)
Path(__file__).with_name('closure-audit.json').write_text(json.dumps(dict(evidence='80-digit conditional stationary exploration, not a global certificate',cases=rows),indent=2)+'\n')
print('checked',len(rows),'small-power conditional profiles')
