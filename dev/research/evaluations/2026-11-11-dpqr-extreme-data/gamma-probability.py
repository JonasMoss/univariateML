"""Independent gamma tails by integrating the exact density, not gammainc.

The endpoint-scaled integrand keeps its log-density difference centered; the
quadrature's numerical error estimate must be below 1e-40 of its positive mass.
"""
import json,math,sys
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit

def log1pmx(x):
 if abs(x)<mp.mpf('1e-10'):
  return mp.fsum((-1)**(k+1)*x**k/k for k in range(2,10))
 return mp.log1p(x)-x

def reference(x,a,s):
 with mp.workdps(1200):
  x,a,s=map(mp.mpf,(x,a,s))
  anchor=-mp.loggamma(a)-a*mp.log(s)+(a-1)*mp.log(x)-x/s
 with mp.workdps(80):
  coefficient=(a*s-x)/s-1
  rate=max(1/(s*mp.sqrt(a)),abs(coefficient/x))
  lower=x<a*s
  sign=-1 if lower else 1
  def integrand(u):
   if not mp.isfinite(u):return mp.mpf(0)
   v=sign*u/(rate*x)
   if v<=-1:return mp.mpf(0)
   return mp.exp(coefficient*v+(a-1)*log1pmx(v))
  mass,error=mp.quad(integrand,[0,1,4,16,mp.inf],error=True)
  if not (mass>0 and error<mp.mpf('1e-40')*mass):raise ArithmeticError(('quadrature',mass,error))
  lp=anchor-mp.log(rate)+mp.log(mass)
  other=mp.log1p(-mp.exp(lp)) if lp<-mp.log(2) else mp.log(-mp.expm1(lp))
  return (lp,other) if lower else (other,lp)

jobs=[];expected=[]
for a in (1e8,1e12,1e24,1e308):
 for s in (1.,1000.,1e-300):
  with mp.workdps(100):center=float(mp.mpf(a)*mp.mpf(s))
  if not 0<center<math.inf:continue
  for x in (center,math.nextafter(center,0),math.nextafter(center,math.inf)):
   lo,hi=reference(x,a,s)
   for lower,b in [(True,lo),(False,hi)]:
    jobs.append(('gamma',1,x,lower,True,[a,s]));expected.append(float(b))
 for z in (-37.,37.):
  if a>1e12:continue
  s=1000.;x=(a+z*math.sqrt(a))*s
  lo,hi=reference(x,a,s)
  for lower,b in [(True,lo),(False,hi)]:
   jobs.append(('gamma',1,x,lower,True,[a,s]));expected.append(float(b))
rows=[dict(job=j,actual=a,expected=b,error=audit.error(a,b)) for j,a,b in zip(jobs,audit.evaluate(jobs),expected)]
(HERE/'gamma-probability.json').write_text(json.dumps(rows,indent=2)+'\n')
print('gamma direct density integration',len(rows),'worst',max(r['error'] for r in rows),flush=True)
for r in rows:
 if r['error']>1e-10:print('FAILED',r,flush=True)
assert all(r['error']<=1e-10 or math.isnan(r['actual']) for r in rows)
