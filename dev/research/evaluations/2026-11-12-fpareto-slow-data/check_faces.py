"""Exact-input, 100-digit closed-form maxima within the reported limit models.
These face optima are not a proof of domination over the full GB2 family.
"""
import argparse,json
from pathlib import Path
import mpmath as mp
mp.mp.dps=100
p=Path(__file__).resolve().parent
cases={c['index']:c for c in json.loads((p/'cases.json').read_text())}
ap=argparse.ArgumentParser();ap.add_argument('--input',default='final-native.jsonl');args=ap.parse_args()
rows={r['index']:r for r in map(json.loads,(p/args.input).read_text().splitlines())}
result=[]
for i in [1599,1600,10717]:
 x=list(map(mp.mpf,cases[i]['data']));z=sorted(map(mp.log,x));n=len(x)
 if i==10717:
  shape=n/sum(z[-1]-v for v in z);ll=n*mp.log(shape)+(shape-1)*sum(z)-n*shape*z[-1]
  model='power';coef=[str(max(x)),str(shape)]
 else:
  best=(-mp.inf,None)
  for m in z:
   L=sum(max(m-v,0) for v in z);R=sum(max(v-m,0) for v in z)
   if not L or not R:continue
   s=mp.sqrt(L)+mp.sqrt(R);ll=n*mp.log(n)-2*n*mp.log(s)-n-sum(z)
   if ll>best[0]:best=ll,[str(mp.exp(m)),str(n/(mp.sqrt(L)*s)),str(n/(mp.sqrt(R)*s))]
  ll,coef=best;model='double Pareto'
 value=float(rows[i]['result'].split()[7]);error=float(abs(ll-mp.mpf(value)))
 result.append(dict(index=i,model=model,face_coef=coef,reference=str(ll),reported=value,absolute_error=error))
 assert error<1e-10
(p/('face-check-double.json' if 'double' in args.input else 'face-check.json')).write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))
