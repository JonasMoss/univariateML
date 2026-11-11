"""Controlled coupled-Gamma power limit: no latent overflow in the reference.

For the listed x below the endpoint the neglected Gamma integrand remainder
is less than exp(-1e288); all moderate quantiles have still smaller error.
At x=scale, Q(a,1)=a E1(1)+O(a*a). Exact endpoint powers use MP products.
"""
import json,math,sys
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent;sys.path.insert(0,str(HERE));import audit
from quality.truth_reference import pair,complement
mp.mp.dps=240;jobs=[];refs=[]
def add(ident,mode,x,lower,logp,pars,want):
 jobs.append((ident,mode,x,lower,logp,pars));refs.append(float(want))
for aa in (1e-310,1e-309,1e-308,1e-307,1e-306):
 for cc in (1e306,1e307,1e308):
  for scale in (1.,1000.):
   a,c,s=map(mp.mpf,(aa,cc,scale));ac=a*c;lg=-mp.euler*a;pars=[aa,cc,scale]
   for ident,sign in [('trgamma',1),('invtrgamma',-1)]:
    for x in (math.nextafter(scale,0.),scale,math.nextafter(scale,math.inf),scale/2,scale*2):
     v=mp.mpf(x)
     if x==scale:
      ld=mp.log(ac)-mp.log(v)-1-lg;tails=pair(mp.log(a)+mp.log(mp.e1(1)),ident=='invtrgamma')
     elif (x<scale)==(ident=='trgamma'):
      lp=sign*ac*mp.log(v/s)-lg;ld=mp.log(ac)-mp.log(v)+lp;tails=pair(lp,ident=='trgamma')
     else:ld=-mp.inf;tails=(mp.mpf(0),-mp.inf) if ident=='trgamma' else (-mp.inf,mp.mpf(0))
     add(ident,0,x,True,True,pars,ld)
     for lower in (True,False):
      lp=tails[0 if lower else 1];add(ident,1,x,lower,True,pars,lp)
      if mp.exp(lp)>mp.mpf('1e-300'):add(ident,1,x,lower,False,pars,mp.exp(lp))
    if ident=='trgamma':
     ld=mp.inf if ac<1 else -mp.inf if ac>1 else -mp.log(s)-lg
     add(ident,0,0.,True,True,pars,ld)
    for lower in (True,False):
     for logp in (True,False):
      for prob in (.1,.5,.9):
       inp=math.log(prob) if logp else prob;lp=mp.mpf(inp) if logp else mp.log(mp.mpf(inp));lp=lp if (ident=='trgamma')==lower else complement(lp)
       q=s*mp.exp(sign*(lp+lg)/ac);add(ident,2,inp,lower,logp,pars,q)
rows=[];failures=[]
for job,got,want in zip(jobs,audit.evaluate(jobs),refs):
 error=audit.error(got,want,job[1]==2 or job[1]==1 and not job[4]);row=dict(job=job,actual=got,expected=want,error=error);rows.append(row)
 if error>(1e-9 if job[1]==2 else 1e-10):failures.append(row)
(HERE/'power-smoke.json').write_text(json.dumps(dict(checks=len(rows),failures=failures,worst=max(r['error'] for r in rows),rows=rows),indent=2)+'\n')
print('coupled power limit',len(rows),'checks;',len(failures),'failures; worst',max(r['error'] for r in rows),flush=True)
if failures:print(failures[:4]);raise SystemExit(1)
