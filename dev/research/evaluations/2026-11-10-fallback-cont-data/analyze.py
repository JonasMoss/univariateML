#!/usr/bin/env python3
from pathlib import Path
import json,collections,math
P=Path(__file__).resolve().parent
cases={str(c['index']):c for c in json.loads((P/'cases.json').read_text())}
def read(name):
 rows=[line.split('\t') for line in (P/name).read_text().splitlines()]
 assert len(rows)==len(cases)
 result={r[0]:r for r in rows};assert len(result)==len(cases)
 return result
before,after=read('before.tsv'),read('after.tsv')
validated={}
for mode,rows in [('before',before),('after',after)]:
 successful=0
 for row in rows.values():
  if row[2] not in ('certified','uncertified'):continue
  assert math.isfinite(float(row[3])),(mode,row)
  for field in row[4:]:
   name,value=field.split('=',1);value=float(value)
   assert math.isfinite(value),(mode,row)
   if name not in ('mean','mu','min'):
    assert value>=0 if name=='a' else value>0,(mode,row)
  successful+=1
 validated[mode]=successful
transitions=[];certified=unchanged=0
for key,c in cases.items():
 a,b=before[key],after[key]
 if a[2]=='certified':
  certified+=1
  if b[2]=='certified' and a[3:]==b[3:]:unchanged+=1
 if a[2]!=b[2]:transitions.append(dict(case=c,before=a[2],after=b[2],fit=b[3:]))
counts={mode:dict(collections.Counter(r[2] for r in rows.values())) for mode,rows in [('before',before),('after',after)]}
summary=dict(successful_finite_domain_rows=validated,cases=len(cases),counts=counts,previously_certified=certified,certified_numeric_unchanged=unchanged,transitions=len(transitions),stop_to_uncertified=sum(t['before'].startswith('stop:') and t['after']=='uncertified' for t in transitions))
(P/'transitions.json').write_text(json.dumps(transitions,indent=2)+'\n')
(P/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
for t in transitions:
 c=t['case'];print(c['index'],c['family'],t['before'],'->',t['after'],c['held'])
