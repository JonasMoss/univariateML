"""Exact binary-input, 240-digit thin logit-normal density and tail checks."""
import json,math,sys
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
mp.mp.dps=240;jobs=[];refs=[]
for x in (math.nextafter(0.,1.),1e-100,.125,.25,.5,.75,.9,math.nextafter(1.,0.)):
 xx=mp.mpf(x);logit=mp.log(xx)-mp.log1p(-xx)
 for sd in (1.,1e-8,1e-12,1e-16,1e-24,1e-100):
  for shift in (-2.,0.,2.):
   mu=float(logit+mp.mpf(sd)*shift);z=(logit-mp.mpf(mu))/mp.mpf(sd);pars=[mu,sd]
   ld=-mp.log(mp.mpf(sd))-mp.log(2*mp.pi)/2-z*z/2-mp.log(xx)-mp.log1p(-xx)
   jobs.append(('logitnorm',0,x,True,True,pars));refs.append(float(ld))
   for lower in (True,False):
    zz=z if lower else -z
    lp=mp.log(mp.erfc(-zz/mp.sqrt(2))/2)
    for logp in (True,False):
     want=float(lp if logp else mp.exp(lp))
     if not logp and want<1e-300:continue
     jobs.append(('logitnorm',1,x,lower,logp,pars));refs.append(want)
rows=[];fail=[];named=[]
for job,actual,expected in zip(jobs,audit.evaluate(jobs),refs):
 error=audit.error(actual,expected,job[1]==1 and not job[4]);r=dict(job=job,actual=actual,expected=expected,error=error);rows.append(r)
 if math.isnan(actual):named.append(r)
 elif error>1e-10:fail.append(r)
(H/'logit-smoke.json').write_text(json.dumps(dict(checks=len(rows),failures=fail,named_precision_regions=named,worst_resolved=max(r['error'] for r in rows if not math.isnan(r['actual'])),rows=rows),indent=2)+'\n')
print('thin logit-normal',len(rows),'checks;',len(fail),'finite failures;',len(named),'named NaNs; worst',max(r['error'] for r in rows if not math.isnan(r['actual'])),flush=True)
if fail:print(fail[:4]);raise SystemExit(1)
