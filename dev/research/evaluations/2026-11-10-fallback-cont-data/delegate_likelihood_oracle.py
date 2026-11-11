#!/usr/bin/env python3
"""High-precision physical likelihood check for Pareto III grid regressions."""
from pathlib import Path
import json
import mpmath as mp
mp.mp.dps=100
P=Path(__file__).resolve().parent
cases={str(c['index']):c for c in json.loads((P/'delegate-cases.json').read_text())}
results=[]
for mode in ['before','after']:
 for line in (P/f'delegate-{mode}.tsv').read_text().splitlines():
  r=line.split('\t');c=cases[r[0]]
  if c['family']!='pareto3' or not r[0].startswith('grid-') or r[2] not in ['certified','uncertified']:continue
  coef={k:mp.mpf(float(v)) for k,v in (x.split('=',1) for x in r[4:])}
  a,s,m=coef['shape'],coef['scale'],coef['min'];ll=mp.mpf(0)
  for v in c['data']:
   x=mp.mpf(v)-m;t=a*mp.log(x/s)
   ll+=mp.log(a)-mp.log(x)-mp.log1p(mp.exp(t))-mp.log1p(mp.exp(-t))
  error=mp.mpf(float(r[3]))-ll
  results.append(dict(mode=mode,case=r[0],certified=r[2]=='certified',stored=float(r[3]),physical=str(ll),error=str(error)))
  if mode=='after' and r[2]=='uncertified':assert abs(error)<mp.mpf('1e-10'),results[-1]
(P/'delegate-likelihood-oracle.json').write_text(json.dumps(results,indent=2)+'\n')
for r in results:print(r['mode'],r['case'],'error',r['error'])
