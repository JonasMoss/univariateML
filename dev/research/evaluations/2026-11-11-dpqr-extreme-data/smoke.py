"""Independent exact-input numerical checks for shared normalization repairs."""
import json,sys,math
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
mp.mp.dps=160
jobs=[];expected=[]
for x in (0.,1.,161.,199.):
 r=mp.mpf(1e8);p=mp.mpf(0.9999999900000001);v=mp.mpf(x)
 jobs.append(('nbinom',0,x,True,True,[float(r),float(p)]))
 expected.append(float(mp.loggamma(r+v)-mp.loggamma(r)-mp.loggamma(v+1)+r*mp.log(p)+v*mp.log1p(-p)))
for ident,pars,x in [('beta',[1e8,1],.99999999),('beta',[.5,1e8],1e-8),('betapr',[1,1e8,1e8],1.),('fpareto',[2,1e8,2,1000,0],1000*(1+.5e-8))]:
 if ident=='beta':
  a,b=map(mp.mpf,pars);v=mp.mpf(x);ld=(a-1)*mp.log(v)+(b-1)*mp.log1p(-v)-mp.log(mp.beta(a,b))
 elif ident=='betapr':
  a,b,s=map(mp.mpf,pars);v=mp.mpf(x);z=mp.log(v/s);ld=-mp.log(v)-mp.log(mp.beta(a,b))-a*mp.log1p(mp.exp(-z))-b*mp.log1p(mp.exp(z))
 else:
  b,c,a,s,mu=map(mp.mpf,pars);v=mp.mpf(x)-mu;z=c*mp.log(v/s);ld=mp.log(c)-mp.log(v)-mp.log(mp.beta(a,b))-a*mp.log1p(mp.exp(-z))-b*mp.log1p(mp.exp(z))
 jobs.append((ident,0,x,True,True,pars));expected.append(float(ld))
for ident,pars,lo in [('pareto3',[1e8,1e8,-1e8],False),('fpareto',[2,1e8,2,1e8,-1e8],True),('unif',[-1,1],True),('nsbeta',[1,1,-1,1],True)]:
 p=mp.exp(mp.mpf(-0.6931471805599453));delta=p-mp.mpf('.5')
 if ident=='pareto3':q=mp.mpf(1e8)*mp.expm1((mp.log1p(-p)-mp.log(p))/mp.mpf(1e8))
 elif ident=='fpareto':
  # The first omitted inverse term is O(delta^3), <1e-48 relative.
  q=mp.mpf(1e8)*mp.expm1((delta/(mp.mpf(3)/8))/mp.mpf(1e8))
 else:q=2*delta
 jobs.append((ident,2,-0.6931471805599453,lo,True,pars));expected.append(float(q))
rows=[]
for job,a,b in zip(jobs,audit.evaluate(jobs),expected):
 e=audit.error(a,b,job[1]==2);rows.append(dict(job=job,actual=a,expected=b,error=e))
(HERE/'normalization-smoke.json').write_text(json.dumps(rows,indent=2)+'\n')
print('shared normalization smoke',len(rows),'worst',max(r['error'] for r in rows),flush=True)
assert all(r['error']<=1e-10 for r in rows)
