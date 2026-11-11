#!/usr/bin/env python3
"""Compare the merged-state and corrected delegate replay without altering old evidence."""
from pathlib import Path
import collections
import json
import math
P=Path(__file__).resolve().parent
cases={str(c['index']):c for c in json.loads((P/'delegate-cases.json').read_text())}
def read(mode):
 rows={r[0]:r for r in (line.split('\t') for line in (P/f'delegate-{mode}.tsv').read_text().splitlines())}
 assert set(rows)==set(cases)
 return rows
before,after=read('before'),read('after')
oracle={(r["mode"],r["case"]):r for r in json.loads((P/"delegate-likelihood-oracle.json").read_text())}
transitions=[];changed=[];same=0;finite=0
for key,c in cases.items():
 a,b=before[key],after[key]
 if a[2]!=b[2]:transitions.append(dict(case=c,before=a[2],after=b[2]))
 if a[2]=='certified' and b[2]=='certified':
  assert a[3:]==b[3:],(key,a,b)
  same+=1
 if b[2] in ('certified','uncertified'):
  finite+=1
  assert math.isfinite(float(b[3]))
  for field in b[4:]:
   k,v=field.split('=',1);v=float(v);assert math.isfinite(v)
   if k not in ('mean','mu','min'):assert v>0
  if a[2] in ('certified','uncertified') and a[3:]!=b[3:]:
   margin=2048*2.220446049250313e-16*(len(c['data'])+abs(float(b[3])))
   reference=float(oracle['before',key]['physical']) if ('before',key) in oracle else float(a[3])
   assert float(b[3])+margin>=reference,(key,a,b,reference)
   changed.append(dict(case=c,before=a[3:],after=b[3:]))
summary=dict(base='6d8a1f06',cases=len(cases),counts={m:dict(collections.Counter(r[2] for r in rows.values())) for m,rows in [('before',before),('after',after)]},stop_to_uncertified=sum(t['before'].startswith('stop:') and t['after']=='uncertified' for t in transitions),lost_flag_corrected=sum(t['before']=='certified' and t['after']=='uncertified' for t in transitions),certified_numeric_unchanged=same,finite_successful_after=finite,changed_previously_successful_numeric=len(changed))
(P/'delegate-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
(P/'delegate-transitions.json').write_text(json.dumps(transitions,indent=2)+'\n')
(P/'delegate-numeric-changes.json').write_text(json.dumps(changed,indent=2)+'\n')
print(json.dumps(summary,indent=2))
