"""Compare native/double/R/WASM stress fits to the independent 70-digit roots."""
from decimal import Decimal,localcontext
from pathlib import Path
import json,sys
D=Path(__file__).resolve().parent
refs={str(v['case']):v for v in json.loads((D/'sstd-stress-reference.json').read_text())}
rows=[]
for mode in ['native','double','r','wasm']:
 suffix='wasm-cli' if mode=='wasm' and '--wasm-cli' in sys.argv else mode
 fits=list(map(json.loads,(D/f'sstd-stress-{suffix}.jsonl').read_text().splitlines()))
 assert len(fits)==2 and {v['id'] for v in fits}==set(refs)
 for f in fits:
  assert not f['error'],(mode,f)
  ref=refs[f['id']]
  errors={}
  with localcontext() as c:
   c.prec=65
   theta=dict(zip(ref['coefficient_names'],map(Decimal,ref['estimates'])))
   for k,v in f['estimates'].items():
    denominator=max(abs(theta[k]),theta['sd']) if k=='mean' else abs(theta[k])
    errors[k]=float(abs(Decimal.from_float(float(v))-theta[k])/denominator)
  assert max(errors.values())<=1e-10,(mode,f,errors)
  rows.append(dict(mode=mode,case=f['id'],normalized_errors=errors,maximum=max(errors.values())))
(D/('sstd-stress-cli-measurements.json' if '--wasm-cli' in sys.argv else 'sstd-stress-measurements.json')).write_text(json.dumps(rows,indent=2)+'\n')
print('PASS: both SSTD stress roots meet the public-coefficient 1e-10 contract in all four modes.')
