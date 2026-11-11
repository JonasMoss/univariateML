"""Measure the explicitly excluded TASK-187 families without editing their files."""
import json,math,sys,collections
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
mp.mp.dps=240;jobs=[];refs=[]
for family in ('invweibull','frechet'):
 for k in (1e8,1e12):
  for scale in (1.,1000.,1e100,1e-100):
   for cancel in ((False,True) if family=='frechet' else (False,)):
    mu=-scale if cancel else 0.
    x=scale*.5/k if cancel else scale*(1+.5/k)
    pars=[k,mu,scale] if family=='frechet' else [k,scale]
    z=(mp.mpf(x)-mp.mpf(mu))/mp.mpf(scale);lh=-mp.mpf(k)*mp.log(z);hazard=mp.exp(lh)
    ld=mp.log(k)-mp.log(scale)-(mp.mpf(k)+1)*mp.log(z)-hazard
    logs=(-hazard,mp.log(-mp.expm1(-hazard)))
    jobs.append((family,0,x,True,True,pars));refs.append(float(ld))
    for lower in (True,False):
     lp=logs[0 if lower else 1]
     for log in (True,False):jobs.append((family,1,x,lower,log,pars));refs.append(float(lp if log else mp.exp(lp)))
rows=[]
for j,a,b in zip(jobs,audit.evaluate(jobs),refs):
 e=audit.error(a,b,j[1]==1 and not j[4]);rows.append(dict(family=j[0],function=('d','p')[j[1]],input=j[2],lower=j[3],log=j[4],parameters=j[5],actual=a,expected=b,error=e))
f=[r for r in rows if r['error']>1e-10]
(H/'external-family-findings.json').write_text(json.dumps(dict(checks=len(rows),failures=f,rows=rows),indent=2)+'\n')
print('TASK-187 measurement',len(rows),'checks;',len(f),'failures;',collections.Counter(r['family'] for r in f),'worst',max(r['error'] for r in rows))
