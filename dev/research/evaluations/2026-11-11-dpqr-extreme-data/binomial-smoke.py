"""600-digit binomial masses, including trial counts beyond exact unit shifts."""
import json,math,sys
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
mp.mp.dps=600;jobs=[];refs=[]
for n in (1e6,1e16,1e100,1e308,math.nextafter(math.inf,0)):
 for p in (.5,1e-5,1/n,math.nextafter(1.,0.)):
  center=float(mp.mpf(n)*mp.mpf(p))
  for k in (0.,1.,4.,16.,float(math.floor(center)),float(math.floor(math.nextafter(center,0))),float(math.floor(math.nextafter(center,math.inf)))):
   if not 0<=k<=n:continue
   nn,pp,kk=map(mp.mpf,(n,p,k));mm=nn-kk
   lp=mp.loggamma(nn+1)-mp.loggamma(kk+1)-mp.loggamma(mm+1)+kk*mp.log(pp)+mm*mp.log1p(-pp)
   jobs.append(('binom',0,k,True,True,[n,p]));refs.append(float(lp))
for r in (16.,1e16,1e100,1e308):
 for p in (.5,1e-5,1e-308):
  mean=mp.mpf(r)*(1-mp.mpf(p))/mp.mpf(p)
  xs=[0.,1.,16.,1e16,1e100,1e308]
  if mean<mp.mpf(str(math.nextafter(math.inf,0))):xs.append(float(mp.floor(mean)))
  for x in xs:
   rr,pp,xx=map(mp.mpf,(r,p,x));lp=mp.loggamma(rr+xx)-mp.loggamma(rr)-mp.loggamma(xx+1)+rr*mp.log(pp)+xx*mp.log1p(-pp)
   jobs.append(('nbinom',0,x,True,True,[r,p]));refs.append(float(lp))
rows=[];fails=[]
for job,actual,expected in zip(jobs,audit.evaluate(jobs),refs):
 error=audit.error(actual,expected);row=dict(job=job,actual=actual,expected=expected,error=error);rows.append(row)
 if error>1e-10:fails.append(row)
(H/'binomial-smoke.json').write_text(json.dumps(dict(checks=len(rows),failures=fails,worst=max(r['error'] for r in rows),rows=rows),indent=2)+'\n')
print('binomial mass',len(rows),'checks;',len(fails),'failures; worst',max(r['error'] for r in rows),flush=True)
if fails:print(fails[:3]);raise SystemExit(1)
