"""Compare captures with the stored decimal references, component by component."""
from pathlib import Path
from decimal import Decimal,localcontext
import json,math
D=Path(__file__).resolve().parent;root=D.parents[3]
rows=[]
for ident in ['slash','sstd','sged','st','bnbinom','gb2']:
 g=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{ident}/golden.json').read_text())
 row={'family':ident,'modes':{}}
 for mode in ['before-native','before-double','after-native','after-double','after-r','after-wasm','after-wasm-cli']:
  p=D/(mode+'.jsonl')
  if not p.exists():continue
  r=next(v for v in map(json.loads,p.read_text().splitlines()) if v['id']==ident)
  if r.get('error'):row['modes'][mode]={'error':r['error']};continue
  with localcontext() as ctx:
   ctx.prec=60
   errors={k:float(abs(Decimal.from_float(float(r['estimates'][k]))-Decimal(v))/(abs(Decimal(v)) or Decimal(1))) for k,v in g['reference']['estimates'].items()}
  row['modes'][mode]={'components':errors,'max_relative':max(errors.values())}
 rows.append(row)
(D/'measurements.json').write_text(json.dumps(rows,indent=2)+'\n')
for r in rows:print(r['family'],{m:v.get('max_relative',v) for m,v in r['modes'].items()})
