#!/usr/bin/env python3
import json
from pathlib import Path
from decimal import Decimal,localcontext
here=Path(__file__).resolve().parent;root=here.parents[3]
def err(a,b):
 if a is None:return float('inf')
 with localcontext() as c:
  c.prec=70;x=Decimal.from_float(float(a));y=Decimal(str(b));return float(abs(x-y)/(abs(y) if y else 1))
summary={}
for mode in ['before-native','after-native','after-double','after-r','after-wasm']:
 p=here/(mode+'.jsonl')
 if not p.exists():continue
 summary[mode]={}
 for r in map(json.loads,p.read_text().splitlines()):
  g=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{r["id"]}/golden.json').read_text())
  summary[mode][r['id']]=dict(error=r['error'],maximum=max([err(v,g['reference']['estimates'][k]) for k,v in r['estimates'].items()] or [float('inf')]))
print(json.dumps(summary,indent=2));(here/'measurements.json').write_text(json.dumps(summary,indent=2)+'\n')
selected=json.loads((here/'replay-cases.json').read_text());replay={}
for mode in ['before','after','after-double','after-wasm','after-r']:
 if not (here/f'replay-{mode}.jsonl').exists():continue
 rows={r['index']:r for r in map(json.loads,(here/f'replay-{mode}.jsonl').read_text().splitlines())};out=[]
 for pair in selected:
  c=pair['case'];ref=pair['reference'];r=rows[c['index']]
  names=list(json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{c["family"]}/golden.json').read_text())['estimates'])
  theta=ref.get('refined_theta',ref.get('better_theta'))
  e={k:err(r['estimates'][k],v) for k,v in zip(names,theta) if k not in c['held']} if r['estimates'] else {}
  out.append(dict(index=c['index'],id=c['family'],sample=c['sample'],held=c['held'],error=r['error'],maximum=max(e.values(),default=None),errors=e))
 replay[mode]=out
(here/'replay-measurements.json').write_text(json.dumps(replay,indent=2)+'\n')
from collections import Counter
for mode,rows in replay.items():
 print(mode,Counter((r['id'],'stop' if r['error'] else 'miss' if r['maximum'] and r['maximum']>1e-10 else 'pass') for r in rows))
for r in replay['after']:
 if r['error'] or r['maximum'] and r['maximum']>1e-10:print(r)
