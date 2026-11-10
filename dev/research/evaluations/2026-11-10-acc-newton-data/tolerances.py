#!/usr/bin/env python3
"""Tighten only this job's eight policies against stored and decimal references."""
import json,math,re,subprocess
from pathlib import Path
from decimal import Decimal,localcontext
h=Path(__file__).resolve().parent;root=h.parents[3]
ids='lomax kumar weibull gumbel fatigue cmb pig foldnorm'.split();maximum={id:0 for id in ids}
for p in h.glob('after-*.jsonl'):
 for r in map(json.loads,p.read_text().splitlines()):
  assert not r['error'],r
  g=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{r["id"]}/golden.json').read_text())
  for k,v in dict(r['estimates'],logLik=r['logLik']).items():
   for ref in [g['logLik'] if k=='logLik' else g['estimates'][k],g['reference']['logLik'] if k=='logLik' else g['reference']['estimates'][k]]:
    with localcontext() as c:
     c.prec=70;x=Decimal.from_float(float(v));y=Decimal(str(ref));error=float(abs(x-y)/(abs(y) if y else 1));maximum[r['id']]=max(maximum[r['id']],error)
allow={}
for id,e in maximum.items():
 assert e<1e-10,(id,e)
 base=10**math.floor(math.log10(e));allow[id]=next(base*k for k in [1,2,3,5,10,20] if base*k>=1.3*e)
p=root/'univariateML-cpp/tests/test_support.hpp';s=p.read_text()
for id,v in allow.items():
 s=re.sub(r'// '+id+r':[^\n]*\n','',s)
 baseline=subprocess.check_output(['git','show','HEAD:univariateML-cpp/tests/test_support.hpp'],cwd=root,text=True)
 old=float(re.search(r'kGolden_'+id+r' = ([^;]+)',baseline)[1]);assert v<=old,(id,v,old)
 s=re.sub(r'(kGolden_'+id+r' = )[^;]+',r'\g<1>'+format(v,'.0e'),s)
p.write_text(s)
p=root/'bindings/r/tests/testthat/test_golden_regression.R';s=p.read_text();a=s.index('GOLDEN_STOP_TOLERANCE')
part=s[:a]
for id,v in allow.items():part=re.sub(r'^  '+id+r' = [^\n]+','  '+id+' = '+format(v,'.0e')+',',part,flags=re.M)
p.write_text(part+s[a:]);(h/'tolerances.json').write_text(json.dumps(dict(measured=maximum,allowances=allow),indent=2)+'\n')
print(allow)
