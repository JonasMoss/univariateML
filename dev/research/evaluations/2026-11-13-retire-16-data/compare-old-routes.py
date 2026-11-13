#!/usr/bin/env python3
"""Compare the saved pre-removal seam replay with old public-family snapshots.

This comparison is numerical at the modifiers-2 audit tolerance, with exact
stop-kind and certification comparisons. The independent before/after seam
comparison is bitwise over every recorded result field.
"""
from pathlib import Path
import gzip,json,math,csv,collections
HERE=Path(__file__).resolve().parent
BASE=HERE.parent/'2026-11-12-fit-snapshot-data'
ROUTES=json.loads((HERE/'route-inputs.json').read_text())
def load(path):
 with gzip.open(path,'rt') as f:return {(r['family'],r['source'],r['case']):r for line in f if (r:=json.loads(line))}
def num(v):
 if v is None:return math.nan
 try:return float.fromhex(v)
 except (TypeError,ValueError):return float(v)
def close(a,b):
 if a==b:return True
 a,b=num(a),num(b)
 return math.isfinite(a) and math.isfinite(b) and abs(a-b)<=1e-10*max(1,abs(a))
known={(r['family'],r['source']):r['category'] for r in csv.DictReader((HERE.parent/'2026-11-12-modifiers-2-data/difference-reasons.csv').open())}
summary={};rows=[]
for mode in ['native','double']:
 old=load(BASE/(mode+'.jsonl.gz'));new=load(HERE/('modifier-routes-'+mode+'.jsonl.gz'))
 counts=collections.Counter();fields=collections.Counter()
 for (id,source,case),record in old.items():
  if id not in ROUTES:continue
  r=ROUTES[id];route=new[(r['id'],source,case)];a=record['result'];b=route['result']
  ea=dict(a.get('estimates',{}));eb=b.get('estimates',{})
  for name in ['a','b','N']:ea.pop(name,None)
  if id=='zmlogarithmic' and 'prob' in ea:ea['theta']=ea.pop('prob')
  differing=[]
  if a['kind']!=b['kind']:differing.append('kind')
  if a['kind']=='ok' and b['kind']=='ok':
   if set(ea)!=set(eb) or any(not close(ea[k],eb[k]) for k in set(ea)&set(eb)):differing.append('coefficients')
   if not close(a['logLik'],b['logLik']):differing.append('logLik')
   if a.get('extra',{}).get('certified')!=b.get('extra',{}).get('certified'):differing.append('certified')
  if not differing:counts['equal']+=1;continue
  reason='Unclassified finding'
  holds=record['held'];data=record.get('data',[])
  archive_reason=known.get((id,'archive-'+str(int(case)))) if source=='archive' else None
  if archive_reason: reason=archive_reason
  elif id=='zipf':
   if num(eb.get('s','nan'))<0 or a['kind']=='nomle' or b['kind']=='limit':reason='widened exponent domain'
   # New magnitude/resample inputs are not the seven independently checked roots.
  elif id in ['tbinom','tpois']:
   if holds.get('a')==0 and holds.get('b')==math.inf:reason='positive full-window extension'
   elif a['kind']!='ok' and b['kind']=='ok' and len(holds)>=len(r['meta']['params']):
    if any(math.isinf(v) for v in holds.values()):reason='proper unbounded window accepted'
  elif id=='tnorm' and a['kind']!='ok' and b['kind']=='ok' and all(p in holds for p in ['mean','sd']):reason='held-window density improvement'
  counts[reason]+=1
  for f in differing:fields[f]+=1
  rows.append(dict(mode=mode,family=id,source=source,case=case,fields=';'.join(differing),reason=reason,old_kind=a['kind'],route_kind=b['kind']))
 summary[mode]={'counts':dict(counts),'fields':dict(fields)}
with (HERE/'old-route-differences.csv').open('w') as f:
 writer=csv.DictWriter(f,fieldnames=list(rows[0]) if rows else ['mode']);writer.writeheader();writer.writerows(rows)
(HERE/'old-route-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
