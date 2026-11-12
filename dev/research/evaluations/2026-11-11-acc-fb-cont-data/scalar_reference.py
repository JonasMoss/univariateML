from pathlib import Path
import json,mpmath as m
m.mp.dps=90
P=Path(__file__).resolve().parent
out={}
for top in [6.0001,6.00000001]:
 x=list(map(m.mpf,[1,1,1,1,top]));n=len(x)
 def score(l):
  A=m.fsum(m.log1p(l*v) for v in x)/n
  return ((1+A)*m.fsum(v/(1+l*v) for v in x)/n-A/l)/l
 l=m.findroot(score,(m.mpf('1e-6') if top<6.00001 else m.mpf('1e-5'),m.mpf('1e-8') if top<6.00001 else m.mpf('1e-6')),tol=m.mpf('1e-80'))
 k=n/m.fsum(m.log1p(l*v) for v in x)
 out[str(top)]={'shape':m.nstr(k,85),'scale':m.nstr(1/l,85),'lambda':m.nstr(l,85)}
(P/'scalar-references.json').write_text(json.dumps(out,indent=2)+'\n');print(json.dumps(out,indent=2))
