"""80-digit score solve, independent arithmetic; not a proof of globality."""
import json
import mpmath as mp
mp.mp.dps=80
x=list(map(mp.mpf,['.03','.1','.3','.7','1','2','5','20']))
p=mp.mpf(2)
def scores(lq,ls):
 q,s=mp.exp(lq),mp.exp(ls)
 return (mp.digamma(p+q)-mp.digamma(q)-mp.fsum(mp.log1p(v/s) for v in x)/len(x),
         -p+(p+q)*mp.fsum(v/(s+v) for v in x)/len(x))
lq,ls=mp.findroot(scores,(mp.log(mp.mpf('.55')),mp.log(mp.mpf('.09'))))
q,s=mp.exp(lq),mp.exp(ls)
rows=[]
for a in map(mp.mpf,['.7','1','2']):
 for inv in [False,True]:
  power=(-1 if inv else 1)/a
  data=[v**power for v in x]
  b=s**power
  pp,qq=(q,p) if inv else (p,q)
  ll=mp.fsum(mp.log(a)-mp.log(v)-mp.log(mp.beta(pp,qq))
       -pp*mp.log1p(mp.exp(-a*mp.log(v/b)))
       -qq*mp.log1p(mp.exp(a*mp.log(v/b))) for v in data)
  rows.append(dict(power=str(a),inverse=inv,shape=mp.nstr(q,65),scale=mp.nstr(b,65),logLik=mp.nstr(ll,65)))
print(json.dumps(rows,indent=2))
