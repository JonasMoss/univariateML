"""Independent 100-digit original-data likelihoods for retained witnesses."""
import argparse,json
from pathlib import Path
import mpmath as mp
mp.mp.dps=100
here=Path(__file__).resolve().parent
cases={r['index']:r for r in json.loads((here/'cases.json').read_text())}
ap=argparse.ArgumentParser();ap.add_argument('--input',default='final-native.jsonl');args=ap.parse_args()
rows={r['index']:r for r in map(json.loads,(here/args.input).read_text().splitlines())}
results=[]
for index in [1599,1600,10717]:
 if index not in rows:continue
 t=rows[index]['result'].split(); count=int(t[11]);pos=12
 for j in range(count):
  size=int(t[pos]);pos+=1;coef=list(map(float,t[pos:pos+size]));pos+=size;lo,hi=map(float,t[pos:pos+2]);pos+=2
  q,a,p,b,shift=map(mp.mpf,coef);x=[mp.mpf(v)-shift for v in cases[index]['data']]
  soft=lambda v:max(v,0)+mp.log1p(mp.exp(-abs(v)))
  ll=sum(mp.log(a)-mp.log(v)-mp.log(mp.beta(p,q))-p*soft(-a*mp.log(v/b))-q*soft(a*mp.log(v/b)) for v in x)
  inside=mp.mpf(lo)<=ll<=mp.mpf(hi)
  results.append(dict(index=index,coefficient=coef,lower=lo,upper=hi,reference=str(ll),inside=bool(inside)))
(here/('witness-check-double.json' if 'double' in args.input else 'witness-check.json')).write_text(json.dumps(results,indent=2)+'\n')
print(json.dumps(results,indent=2));assert len(results)==3 and all(r['inside'] for r in results)
