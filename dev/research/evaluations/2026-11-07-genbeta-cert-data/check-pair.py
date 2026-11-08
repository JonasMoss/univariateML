#!/usr/bin/env python3
"""Independent 70-digit stationary coefficients on the frozen sample.

The global claim uses the interval proof, not findroot. argv[1] is the
compiled pair-oracle.cpp executable; argv[2] is output JSON, or '-'.
"""
import json
from pathlib import Path
import subprocess
import sys
import mpmath as mp
mp.mp.dps=70
root=Path(__file__).resolve().parents[4]
g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/genbeta/golden.json').read_text())
x=g['data']
y=[-mp.log(mp.mpf(v)) for v in x]
m=sum(y)/len(y)
text=str(len(x))+'\n'+' '.join(map(repr,x))+'\n'
output=subprocess.check_output([sys.argv[1]],input=text,text=True)
rows=[]
for line in output.splitlines():
    p0,q0,a0,ll0,status,work,reason=map(float,line.split())
    assert status==0,line
    p=mp.mpf(p0)
    def scores(la,lq):
        a,q=mp.exp(la),mp.exp(lq)
        r=-sum(mp.log(-mp.expm1(-a*v)) for v in y)/len(y)
        d=sum(a*v/mp.expm1(a*v) for v in y)/len(y)
        return 1-p*a*m+(q-1)*d,mp.digamma(p+q)-mp.digamma(q)-r
    la,lq=mp.findroot(scores,(mp.log(a0),mp.log(q0)),tol=mp.mpf('1e-55'),maxsteps=50)
    a,q=mp.exp(la),mp.exp(lq)
    ae,qe=abs(mp.mpf(a0)/a-1),abs(mp.mpf(q0)/q-1)
    assert max(ae,qe)<mp.mpf('1e-9'),(line,ae,qe)
    ll=sum(mp.log(a)-mp.log(mp.beta(p,q))-p*a*v+(q-1)*mp.log(-mp.expm1(-a*v))+v for v in y)
    assert abs(mp.mpf(ll0)-ll)<mp.mpf('1e-8')
    rows.append(dict(p=p0,q=q0,power=a0,power_relative_error=float(ae),
                     q_relative_error=float(qe),loglik_absolute_error=float(abs(mp.mpf(ll0)-ll)),work=int(work)))
out=dict(evidence='70-digit local stationary oracle; global completeness is the separate interval certificate',cases=rows)
s=json.dumps(out,indent=2)+'\n'
if sys.argv[2]=='-':print(s,end='')
else:Path(sys.argv[2]).write_text(s)
