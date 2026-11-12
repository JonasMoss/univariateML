#!/usr/bin/env python3
"""Retain decimal, independent density sums for every baseline violation.

Run after the two censuses. It imports scalar symbolic laws, never C++ density
or fitted-likelihood code. All numbers enter mpmath as exact binary64 values.
"""
import json,sys
from pathlib import Path
import mpmath as mp
root=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(root/'dev/validation'))
from quality.truth import read,declarative,module_at,number,DISTS
scratch=root/'build/scratch/seam-rerun'
result=[]
for mode in ('native','double'):
 for line in (scratch/f'before-{mode}.jsonl').read_text().splitlines():
  r=json.loads(line)
  if r['outcome']!='mismatch':continue
  meta,_=read(DISTS/r['family']);theta=dict(zip([p['name'] for p in meta['params']],r['parameters']))
  values=[]
  for digits in (150,250):
   mp.mp.dps=digits
   law=declarative(DISTS/r['family'],meta,theta,module_at(DISTS/r['family']/'truth.py'))
   terms=[law(number(x)) for x in r['data']];value=mp.fsum(terms)
   values.append(dict(digits=digits,sum=str(value),terms=[str(v) for v in terms]))
  agreement=abs(mp.mpf(values[0]['sum'])-mp.mpf(values[1]['sum']))/max(1,abs(mp.mpf(values[1]['sum'])))
  assert agreement<mp.mpf('1e-120'),(mode,r['family'],r['index'],agreement)
  result.append(dict(mode=mode,**r,high_precision=values,relative_precision_disagreement=str(agreement)))
Path(__file__).with_name('high-precision.json').write_text(json.dumps(result,indent=2)+'\n')
print('Independent decimal references:',len(result))
