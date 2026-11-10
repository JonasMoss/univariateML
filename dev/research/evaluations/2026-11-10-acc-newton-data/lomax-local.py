#!/usr/bin/env python3
"""Independent 90-digit profile roots for the double-intermediate regressions."""
import mpmath as m,json
from pathlib import Path
m.mp.dps=90
rows=[]
for data,start in [([1,100],m.mpf('.77')),([1,29],m.mpf('.237')),([1,1,1,12,12],m.mpf('.036'))]:
 x=list(map(m.mpf,data));n=len(x)
 def score(rate):
  S=sum(m.log1p(rate*v) for v in x);return n/rate-(n/S+1)*sum(v/(1+rate*v) for v in x)
 rate=m.findroot(score,(start*m.mpf('.99'),start*m.mpf('1.01')))
 shape=n/sum(m.log1p(rate*v) for v in x)
 rows.append(dict(data=data,lambda_=str(rate),kappa=str(shape),score=str(score(rate)),curvature=str(m.diff(score,rate)),precision=90))
Path(__file__).with_suffix('.json').write_text(json.dumps(rows,indent=2)+'\n');print(json.dumps(rows,indent=2))
