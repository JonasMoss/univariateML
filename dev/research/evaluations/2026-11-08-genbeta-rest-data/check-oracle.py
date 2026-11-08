#!/usr/bin/env python3
"""70-digit stationary oracle; global support comes from held-q-proof.md."""
import json,subprocess,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
root=Path(__file__).resolve().parents[4]
x=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/genbeta/golden.json').read_text())['data']
y=[-mp.log(mp.mpf(v)) for v in x];m=sum(y)/len(y)
out=subprocess.check_output([sys.argv[1]],input=str(len(x))+'\n'+' '.join(map(repr,x))+'\n',text=True)
rows=[]
for line in out.splitlines():
 q0,p0,a0,ll0,status,reason,work=map(float,line.split());assert status==0,line
 q=mp.mpf(q0)
 def scores(lp,la):
  p,a=mp.exp(lp),mp.exp(la)
  return mp.digamma(p+q)-mp.digamma(p)-a*m,1-p*a*m+(q-1)*sum(a*v/mp.expm1(a*v) for v in y)/len(y)
 lp,la=mp.findroot(scores,(mp.log(p0),mp.log(a0)),tol=mp.mpf('1e-55'),maxsteps=50)
 p,a=mp.exp(lp),mp.exp(la);pe=abs(mp.mpf(p0)/p-1);ae=abs(mp.mpf(a0)/a-1)
 ll=sum(mp.log(a)-mp.log(mp.beta(p,q))-p*a*v+(q-1)*mp.log(-mp.expm1(-a*v))+v for v in y)
 assert max(pe,ae)<mp.mpf('1e-9'),(line,pe,ae)
 assert abs(ll0-ll)<mp.mpf('1e-8')
 rows.append(dict(q=q0,p=p0,power=a0,p_relative_error=float(pe),power_relative_error=float(ae),logLik_absolute_error=float(abs(ll0-ll)),work=int(work)))
result=dict(evidence='70-digit local stationary oracle; no independent global claim',cases=rows)
Path(sys.argv[2]).write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))
