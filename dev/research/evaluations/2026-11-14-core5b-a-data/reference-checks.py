#!/usr/bin/env python3
"""Independent 100/150+ digit likelihood checks for the assigned strict density entries.

Uses fit-density's symbolic mpmath reference, independently of the fitter and
C++ density; precision rises for subnormal or strongly concentrated data.
"""
from pathlib import Path
import sys,importlib.util,json,gzip,collections,math
ROOT=Path(__file__).resolve().parents[4];sys.path.insert(0,str(ROOT/'dev/validation'))
def imp(name,file):
 spec=importlib.util.spec_from_file_location(name,ROOT/'dev/validation'/file)
 module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module
fd=imp('density','fit-density.py');fs=fd.fs
archive=collections.defaultdict(list)
for row in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())):archive[row['family']].append(row)
known=json.loads((Path(__file__).parent/'known-before.json').read_text());table=fd.laws();records=[];inputs={};cache={}
for mode in ['native','double']:
 rows=fd.load(str(ROOT/'build/snapshots'/mode))
 for key,entry in known.items():
  check,which,family,source,case=key.split('|')
  if which!=mode or entry['ref'] not in ['TASK-214.15','TASK-214.16']:continue
  row=rows[family,source,case];result=row['result']
  inputs.setdefault(family,fd.inputs(family,archive)[1])
  data=inputs[family][source,case][0]['data']
  items=fd.items(row,family,table)
  if not items:
   records.append(dict(key=key,kind=result['kind'],reason=result['reason'],checked=False));continue
  label,law,values,reported,bounds,own=items[0]
  if law is None:
   records.append(dict(key=key,checked=False,model=result['model']));continue
  ck=(law,tuple(sorted(values.items())),tuple(data))
  if ck not in cache:cache[ck]=fd.attribute(law,values,data)
  ref,reason=cache[ck]
  relative=fd.relative(reported,ref) if ref is not None else None
  records.append(dict(key=key,law=law,reported=reported,reference=ref,relative=relative,checked=True,pass_=relative is not None and relative<1e-10,reference_error=reason))
file=Path(__file__).parent/'independent-likelihoods.json'
file.write_text(json.dumps(records,indent=2,allow_nan=False)+'\n')
failed=[r for r in records if r.get('checked') and not r.get('pass_')]
print(f'{len(records)} original strict density entries; {sum(r.get("checked",False) for r in records)} independent comparisons; {len(failed)} failures')
for r in failed:print(r)
raise SystemExit(bool(failed))
