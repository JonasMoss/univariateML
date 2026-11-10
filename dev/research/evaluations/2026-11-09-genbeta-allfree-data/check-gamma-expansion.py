"""Independent 90-digit profile expansion check; run through the CPU wrapper."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=90
data=Path('dev/research/evaluations/2026-11-09-genbeta-allfree-data')
samples=[[mp.mpf(x) for x in row] for row in [('.1','.3','1','2.6'),('.5','1.5'),('.01','.1','.5','3.39'),('.2','.6','1.1','2.1')]]
rows=[]
for sample,y in enumerate(samples):
 mu=sum(y)/len(y);v=[z/mu for z in y];c=-sum(mp.log(z) for z in v)/len(v)
 lo,hi=1/(2*c),1/c
 for _ in range(320):
  k=(lo+hi)/2
  if mp.log(k)-mp.digamma(k)>c:lo=k
  else:hi=k
 k=(lo+hi)/2;G=k*mp.log(k)-k-mp.loggamma(k)-(k-1)*c
 b=(k-1)/2*(k/k-1);assert b==0
 J=mp.matrix([[mp.polygamma(1,k),-1/k],[-1/k,1/k]])
 db=mp.matrix([(k-1)/(2*k),-(k-1)/(2*k)])
 motion=mp.lu_solve(J,db)
 assert abs(motion[0])<mp.mpf('1e-80')
 assert abs(motion[1]+(k-1)/2)<mp.mpf('1e-80')
 c2=(k-1)*(sum(z*z for z in v)/len(v)-(k+1)/k)/24
 errors=[]
 for h in [mp.mpf('0.001'),mp.mpf('0.0001'),mp.mpf('0.00001')]:
  r=-sum(mp.log(-mp.expm1(-h*z)) for z in v)/len(v)
  def score(lp,lq):
   p,q=mp.exp(lp),mp.exp(lq)
   return mp.digamma(p+q)-mp.digamma(p)-h,mp.digamma(p+q)-mp.digamma(q)-r
  lp,lq=mp.findroot(score,(mp.log(k/h-(k-1)/2),mp.log(k)),tol=mp.mpf('1e-75'),maxsteps=100)
  p,q=mp.exp(lp),mp.exp(lq)
  F=mp.log(h)-p*h-(q-1)*r-mp.log(mp.beta(p,q))
  pc=k/h-(k-1)/2
  feasible=mp.log(h)-pc*h-(k-1)*r-mp.log(mp.beta(pc,k))
  lower_constant=k/6+max(mp.mpf(0),1-k)*sum(z*z for z in v)/len(v)/24
  assert feasible>=G-lower_constant*h*h
  assert F>=feasible-mp.mpf('1e-70')
  err=abs((F-G)/(h*h)-c2);errors.append(err)
 assert errors[-1]<errors[0]/1000
 rows.append(dict(sample=sample,shape=str(k),c1=str(b),c2=str(c2),scaled_quadratic_errors=list(map(str,errors)),passed=True))
(data/'gamma-expansion.json').write_text(json.dumps(dict(precision=90,checks=rows),indent=2)+'\n')
print('4 profile expansions and information adjustments passed at 90 digits; c1=0 identically')
