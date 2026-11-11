#!/usr/bin/env python3
"""Independently solve the shallow GB1 fixture's analytic stationary equations.

Input: first line fitted p,q,power,ll; remaining lines the exact binary64 data
printed by the C++ fixture generator (build/scratch/genbeta-shallow).
Run under the CPU slot wrapper; no fitted routine is used in this solve.
"""
import json,sys
from pathlib import Path
import mpmath as mp
here=Path(__file__).resolve().parent
lines=Path(sys.argv[1]).read_text().splitlines()
mp.mp.dps=90
initial=list(map(mp.mpf,lines[0].split()))
x=[mp.mpf(float(v)) for v in lines[1:]];y=[-mp.log(v) for v in x];n=len(y);sy=sum(y)
def score(u,v,w):
 a,b,t=mp.exp(u),mp.exp(v),mp.exp(w)
 return (a*(-t*sy+n*(mp.digamma(a+b)-mp.digamma(a))),
         b*(sum(mp.log(-mp.expm1(-t*z)) for z in y)+n*(mp.digamma(a+b)-mp.digamma(b))),
         n-a*t*sy+(b-1)*sum(t*z/mp.expm1(t*z) for z in y))
root=mp.findroot(score,tuple(mp.log(v) for v in initial[:3]),tol=mp.mpf('1e-80'),maxsteps=100)
a,b,t=[mp.exp(v) for v in root]
ll=n*(mp.log(t)-mp.loggamma(a)-mp.loggamma(b)+mp.loggamma(a+b))-a*t*sy+(b-1)*sum(mp.log(-mp.expm1(-t*z)) for z in y)+sum(y)
result=dict(precision_digits=90,shape1=str(a),shape2=str(b),shape3=str(t),logLik=str(ll),score=[str(v) for v in score(*root)])
(here/(sys.argv[2] if len(sys.argv)>2 else 'shallow-reference.json')).write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
