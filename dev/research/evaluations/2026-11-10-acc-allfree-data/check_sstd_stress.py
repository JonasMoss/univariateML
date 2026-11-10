"""Independent 70-digit roots for the ordinary full-tier SSTD stress cases.

Use the README density directly, with exact binary64 inputs. Replication by
100 multiplies this objective by 100 and therefore preserves its maximizer.
"""
import json,mpmath as m
from pathlib import Path
m.mp.dps=70
D=Path(__file__).resolve().parent
cases=list(map(json.loads,(D/'sstd-full-tier-cases.jsonl').read_text().splitlines()))
result=[]
for name,initial in [(5007,['-.078716110324341976','.93908150113827826','84.0901151434594','1.5751162740149001']),('replication',['0','.99657762469454403','4.0462191831264356','1'])]:
 x=list(map(lambda v:m.mpf(float(v)),next(c['data'] for c in cases if c['seed']==name)))
 def objective(*p):
  mu,ls,ln,lxi=p;sigma=m.exp(ls);nu=2+m.exp(ln);xi=m.exp(lxi)
  absolute=m.exp(m.loggamma((nu-1)/2)-m.loggamma(nu/2))*m.sqrt((nu-2)/m.pi)
  variance=m.sqrt((1-absolute**2)*(xi**2+xi**-2)+2*absolute**2-1)
  shift=absolute*(xi-1/xi)
  constant=ls-m.log(variance)+m.log(xi+1/xi)-m.log(2)-m.loggamma((nu+1)/2)+m.loggamma(nu/2)+m.log((nu-2)*m.pi)/2
  f=len(x)*constant
  for v in x:
   z=(v-mu)*variance/sigma+shift;w=z/xi if z>0 else z*xi
   f+=(nu+1)/2*m.log1p(w*w/(nu-2))
  return f
 def score(*p):
  out=[]
  for i in range(4):
   def one(t):
    q=list(p);q[i]=t;return objective(*q)
   out.append(m.diff(one,p[i]))
  return out
 physical=list(map(m.mpf,initial));start=[physical[0],m.log(physical[1]),m.log(physical[2]-2),m.log(physical[3])]
 root=list(m.findroot(score,tuple(start),tol=m.mpf('1e-48'),maxsteps=20))
 H=m.matrix(4)
 for j in range(4):
  for k in range(4):
   def pair(a,b):
    p=root[:];p[j]=a;p[k]=b;return objective(*p)
   if j==k:
    def one(t):
     p=root[:];p[j]=t;return objective(*p)
    H[j,k]=m.diff(one,root[j],2)
   else:H[j,k]=m.diff(pair,(root[j],root[k]),(1,1))
 eig=list(m.eigsy(H,eigvals_only=True))
 estimates=[root[0],m.exp(root[1]),2+m.exp(root[2]),m.exp(root[3])]
 s=score(*root);assert max(abs(v) for v in s)<m.mpf('1e-40');assert min(eig)>0
 row=dict(case=name,n=len(x),precision_digits=70,coefficient_names=['mean','sd','nu','xi'],estimates=list(map(str,estimates)),logLik=str(-objective(*root)),maximum_absolute_score=str(max(abs(v) for v in s)),information_eigenvalues=list(map(str,eig)))
 result.append(row);print(json.dumps(row),flush=True)
(D/'sstd-stress-reference.json').write_text(json.dumps(result,indent=2)+'\n')
