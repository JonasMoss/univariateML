#!/usr/bin/env python3
"""Independent high-precision density values for every scoped old density defect."""
import importlib.util,json,sys,math,gzip
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('density',ROOT/'dev/validation/fit-density.py')
d=importlib.util.module_from_spec(spec);spec.loader.exec_module(d)
DEST=Path(__file__).resolve().parent
known=json.loads((DEST/'baseline-known.json').read_text());table=d.laws();archive={};out=[]
for c in json.loads(gzip.decompress(d.fs.ARCHIVE.read_bytes())):archive.setdefault(c["family"],[]).append(c)
for mode in ['native','double']:
 rows=d.load(ROOT/f'build/snapshots/{mode}')
 for key,entry in known.items():
  check,m,f,source,case=key.split('|')[:5]
  if m!=mode or not check.startswith('density'):continue
  row=rows[(f,source,case)];_,inputs=d.inputs(f,archive);data=inputs[(source,case)][0]['data']
  for label,law,values,reported,bounds,own in d.items(row,f,table):
   ref,error=d.attribute(law,values,data)
   item=dict(mode=mode,family=f,source=source,case=case,law=law,coefficients=values,reported=reported,reference=ref,error=error)
   if error is None:
    item['relative_error']=d.relative(reported,ref)
    assert item['relative_error']<=1e-10,item
   else:raise AssertionError(item)
   out.append(item)
(DEST/'reference-points.json').write_text(json.dumps(out,indent=2)+'\n')
print(f'{len(out)} represented point likelihoods agree with independent adaptive-precision references.')
