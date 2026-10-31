"""Reproduce the approved wasm tolerance evidence; requires existing mpmath.
Run from the repository root: python3 bindings/js/validation/check-wasm-scores.py
Frozen wasm outputs are compared with canonical, unchanged golden data.
"""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=60

def exact(v):
 if isinstance(v,float):
  n,d=v.as_integer_ratio();return mp.mpf(n)/d
 return mp.mpf(v)

def burr(data,p):
 k,c,s=(exact(p[n]) for n in ('shape1','shape2','scale'))
 n=len(data);ll=mp.mpf(0);scores=[mp.mpf(0)]*3
 for x in data:
  l=mp.log(exact(x)/s);y=mp.exp(c*l);t=y/(1+y)
  ll+=mp.log(k)+mp.log(c)-mp.log(s)+(c-1)*l-(k+1)*mp.log1p(y)
  scores[0]+=1/k-mp.log1p(y)
  scores[1]+=1/c+l-(k+1)*t*l
  scores[2]+=(-c+(k+1)*c*t)/s
 return ll,mp.sqrt(sum(v*v for v in scores))

def bhatt(data,p):
 mu,s,a=(exact(p[n]) for n in ('mu','sigma','a'))
 ll=mp.mpf(0);scores=[mp.mpf(0)]*3
 for x in data:
  plus=(exact(x)-mu+a)/s;minus=(exact(x)-mu-a)/s
  pp=mp.exp(-plus*plus/2)/mp.sqrt(2*mp.pi)
  pm=mp.exp(-minus*minus/2)/mp.sqrt(2*mp.pi)
  d=(mp.erf(plus/mp.sqrt(2))-mp.erf(minus/mp.sqrt(2)))/2
  ll+=mp.log(d)-mp.log(2*a)
  scores[0]+=(pm-pp)/(s*d)
  scores[1]+=(minus*pm-plus*pp)/(s*d)
  scores[2]+=(pp+pm)/(s*d)-1/a
 return ll,mp.sqrt(sum(v*v for v in scores))

def pareto4(data,p):
 # The threshold is held; evaluate the three free Burr coordinates.
 return burr([exact(x)-exact(p['min']) for x in data],p)

out=[]
for row in json.loads(Path('bindings/js/validation/wasm-golden-results.json').read_text()):
 row['g']=json.loads(Path(f"univariateML-cpp/include/cppunivariate/distributions/{row['id']}/golden.json").read_text())
 fn=globals()[row['id']];g=row['g'];r=row['result']
 ll,score=fn(g['data'],r['estimates']);baseline,baseline_score=fn(g['data'],g['estimates'])
 out.append(dict(id=row['id'],dps=mp.mp.dps,coordinates=[name for name in r['estimates'] if name not in r.get('fixed',[])],normalization='total log likelihood and free total score in declared coefficients',wasm_logLik=mp.nstr(ll,30),golden_logLik=mp.nstr(baseline,30),logLik_difference=mp.nstr(ll-baseline,15),wasm_score_norm=mp.nstr(score,15),golden_score_norm=mp.nstr(baseline_score,15),reported_logLik_error=mp.nstr(exact(r['logLik'])-ll,15)))
# Print the evidence without modifying the frozen inputs.
print(json.dumps(out,indent=2))
