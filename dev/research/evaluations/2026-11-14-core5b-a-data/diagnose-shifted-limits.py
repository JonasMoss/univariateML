#!/usr/bin/env python3
"""Quantify the shared transform-metadata gap; this does not bless those payloads."""
from pathlib import Path
import json
import mpmath as mp
D=Path(__file__).parent;rows=json.loads((D/'shifted-limit-payloads.json').read_text());records=[]
for r in rows:
 assert r['kind']=='limit'
 with mp.workdps(100):
  p={k:mp.mpf(v) for k,v in r['estimates'].items()};minimum=mp.mpf(r['held']['min'])
  def value(shift):
   terms=[]
   for x in r['data']:
    y=mp.mpf(x)-shift
    if r['model']=='trgamma':
     a,b,s=p['shape1'],p['shape2'],p['scale']
     terms.append(mp.log(b)+(a*b-1)*mp.log(y)-a*b*mp.log(s)-mp.loggamma(a)-(y/s)**b)
    elif r['model']=='exp':terms.append(mp.log(p['rate'])-p['rate']*y)
    else:raise RuntimeError('Unexpected model '+r['model'])
   return mp.fsum(terms)
  shifted,unshifted=value(minimum),value(0);reported=mp.mpf(r['limit_loglik'])
  assert abs(reported-shifted)<mp.mpf('1e-10')*max(1,abs(shifted))
  assert abs(reported-unshifted)>mp.mpf('.01')
  records.append(dict(family=r['family'],model=r['model'],reported=r['limit_loglik'],shifted_reference=mp.nstr(shifted,80),base_model_on_original_data=mp.nstr(unshifted,80),missing_offset=r['held']['min']))
(D/'shifted-limit-diagnosis.json').write_text(json.dumps(records,indent=2)+'\n')
print('Two base-model payloads need shared coordinate-transform metadata; see shifted-limit-diagnosis.json.')
