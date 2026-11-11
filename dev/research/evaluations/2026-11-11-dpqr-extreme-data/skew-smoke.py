"""240-digit exact-input d/p checks after independent two-piece transformations."""
import json,math,sys,signal
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
def reference_timeout(*args):raise TimeoutError('300-second independent skew-reference budget')
signal.signal(signal.SIGALRM,reference_timeout);signal.alarm(300)
mp.mp.dps=240;jobs=[];refs=[]
for ident,nus in [('sged',(.3,1.,2.,3.8792797496374307,5.,30.,1e8)),('sstd',(2.00000001,2.5492086449176115,3.093978716511268,5.,1e8))]:
 for nu0 in nus:
  nu=mp.mpf(nu0)
  if ident=='sged':
   ls=(mp.loggamma(1/nu)-mp.loggamma(3/nu))/2;moment=mp.exp(mp.loggamma(2/nu)-(mp.loggamma(1/nu)+mp.loggamma(3/nu))/2)
  else:moment=mp.sqrt(nu-2)/mp.sqrt(mp.pi)*mp.exp(mp.loggamma((nu-1)/2)-mp.loggamma(nu/2))
  for xi0 in (1e-12,1e-8,1.,3.,1e8,1e12):
   xi=mp.mpf(xi0);shift=moment*(xi-1/xi);sigma=mp.sqrt((1-moment*moment)*(xi*xi+1/(xi*xi))+2*moment*moment-1)
   for mean0,sd0 in ((0.,1.),(-1000.,1000.)):
    mean,sd=map(mp.mpf,(mean0,sd0))
    for vv in (-10.,-1.,-1e-6,0.,1e-6,1.,10.):
     latent=mp.mpf(vv);skew=latent*(xi if latent>=0 else 1/xi);x=float(mean+sd*(skew-shift)/sigma)
     z=(mp.mpf(x)-mean)/sd*sigma+shift;v=abs(z/(xi if z>=0 else 1/xi))
     if ident=='sged':
      logy=nu*(mp.log(v)-ls) if v else -mp.inf
      if logy>1000:base=-mp.inf;logupper=-mp.inf
      else:
       y=mp.exp(logy);base=-mp.log(2)-ls-mp.loggamma(1+1/nu)-y
       if logy < -1000:
        lower=mp.exp(logy/nu-mp.loggamma(1+1/nu));logupper=mp.log1p(-lower)-mp.log(2)
       else:logupper=mp.log(mp.gammainc(1/nu,y,mp.inf,regularized=True))-mp.log(2)
     else:
      base=mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log((nu-2)*mp.pi)/2-(nu+1)/2*mp.log1p(v*v/(nu-2))
      if nu>1e6 and v<20:
       central=mp.betainc(mp.mpf('.5'),nu/2,0,v*v/(nu-2+v*v),regularized=True);logupper=mp.log1p(-central)-mp.log(2)
      else:logupper=mp.log(mp.betainc(nu/2,mp.mpf('.5'),0,(nu-2)/(nu-2+v*v),regularized=True))-mp.log(2)
     ld=mp.log(2/(xi+1/xi))+base-mp.log(sd)+mp.log(sigma)
     logsmall=mp.log(2/(xi+1/xi))+mp.log(xi if z>=0 else 1/xi)+logupper
     logs=(logsmall,mp.log1p(-mp.exp(logsmall))) if z<0 else (mp.log1p(-mp.exp(logsmall)),logsmall)
     pars=[mean0,sd0,nu0,xi0];jobs.append((ident,0,x,True,True,pars));refs.append(float(ld))
     for lower in (True,False):
      lp=logs[0 if lower else 1];jobs.append((ident,1,x,lower,True,pars));refs.append(float(lp))
      natural=float(mp.exp(lp))
      if natural>1e-300:jobs.append((ident,1,x,lower,False,pars));refs.append(natural)
rows=[];fail=[];named=[]
for job,actual,expected in zip(jobs,audit.evaluate(jobs),refs):
 error=audit.error(actual,expected,job[1]==1 and not job[4]);r=dict(job=job,actual=actual,expected=expected,error=error);rows.append(r)
 if math.isnan(actual):named.append(r)
 elif error>1e-10:fail.append(r)
(H/'skew-smoke.json').write_text(json.dumps(dict(checks=len(rows),failures=fail,named_precision_regions=named,worst_resolved=max(r['error'] for r in rows if not math.isnan(r['actual'])),rows=rows),indent=2)+'\n')
print('two-piece d/p',len(rows),'checks;',len(fail),'finite failures;',len(named),'named NaNs; worst',max(r['error'] for r in rows if not math.isnan(r['actual'])),flush=True)
if fail:print(fail[:4]);raise SystemExit(1)
