#!/usr/bin/env python3
"""90-digit independent half-t stationary references for five seeded cases."""
import json
from pathlib import Path
import mpmath as mp
here=Path(__file__).resolve().parent;mp.mp.dps=90
out=[]
for r in map(json.loads,(here/'ht-seeded.jsonl').read_text().splitlines()):
 assert r['status']==0,r
 x=[mp.mpf(v) for v in r['data']];n=len(x)
 def score(u,v):
  nu,s=mp.exp(u),mp.exp(v)
  a=[z*z/(nu*s*s) for z in x];w=[z/(1+z) for z in a]
  return (n*nu/2*(mp.digamma((nu+1)/2)-mp.digamma(nu/2)-1/nu)-nu/2*sum(mp.log1p(z) for z in a)+(nu+1)/2*sum(w),-n+(nu+1)*sum(w))
 q=mp.findroot(score,(mp.log(r['nu']),mp.log(r['sigma'])),tol=mp.mpf('1e-80'),maxsteps=100)
 ref=[mp.exp(z) for z in q]
 err=lambda v:float(max(abs(mp.mpf(a)/b-1) for a,b in zip(v,ref)))
 out.append(dict(index=r['index'],reference=[str(v) for v in ref],before_error=err(r['before']),after_error=err([r['nu'],r['sigma']]),score=[str(v) for v in score(*q)]))
 assert out[-1]['after_error']<=1e-10,out[-1]
assert len(out)==5,('missing seeded cases',len(out))
(here/'ht-references.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
