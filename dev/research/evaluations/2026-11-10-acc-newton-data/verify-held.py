"""Check all conditional probes against independent roots or lattice failures."""
import json
from pathlib import Path
from decimal import Decimal,localcontext
h=Path(__file__).resolve().parent
cases=json.loads((h/'held-cases.json').read_text());refs=json.loads((h/'held-reference.json').read_text());out={}
for mode in ['native','double','r','wasm']:
 rows={r['id']:r for r in map(json.loads,(h/f'held-probes-{mode}.jsonl').read_text().splitlines())}
 assert len(rows)==len(cases),(mode,len(rows))
 out[mode]={}
 for c in cases:
  r=rows[c['id']]
  if c.get('numerical'):
   assert r['status']==4 if mode in ['native','double'] else r['kind']=='numerical',(mode,r)
   out[mode][c['id']]={'outcome':'numerical-accuracy failure'}
  else:
   assert r['status']==0 if mode in ['native','double'] else not r['error'],(mode,r)
   key='shape' if c['family']=='weibull' else 'lambda'
   value=r[key] if mode in ['native','double'] else r['estimates'][key]
   with localcontext() as ctx:
    ctx.prec=70;x=Decimal.from_float(float(value));y=Decimal(refs[c['id']][key]);error=float(abs(x-y)/abs(y))
   assert error<=1e-10,(mode,c['id'],error)
   out[mode][c['id']]={'outcome':'verified','maximum':error}
(h/'held-measurements.json').write_text(json.dumps(out,indent=2)+'\n')
print('PASS: four conditional roots and three numerical-accuracy failures in all four modes')
