#!/usr/bin/env python3
"""Ensure distinct locations in the nearly constant continuous-data sweep."""
import json,math,pathlib
import audit
HERE=pathlib.Path(__file__).resolve().parent
cases=json.loads((HERE/'cases.json').read_text());cases=[c for c in cases if not c['sample'].startswith('constant-extra-')]
added=0
for folder,m,g in audit.models():
 if m['support']['type']=='integer':continue
 old=[c for c in cases if c['family']==m['id']]
 locations={c['data'][2] for c in old if c['sample'].startswith('constant-') and len(c['data'])==5}
 if len(locations)>=2:continue
 env={k:audit.number(v) for k,v in g['estimates'].items()};env.update(inf=math.inf,Inf=math.inf,min=min,max=max,floor=math.floor,ceil=math.ceil)
 loc=1. if 1. not in locations else 2.
 try:
  lo=float(eval(str(m['support']['lower']),{'__builtins__':{}},env))
  hi=float(eval(str(m['support']['upper']),{'__builtins__':{}},env))
  if loc<=lo:loc=lo+1
  if loc>=hi:loc=(lo+hi)/2 if math.isfinite(lo) else hi-1
 except Exception:pass
 seen=set()
 for c in old:
  if c['mask'] in seen:continue
  seen.add(c['mask'])
  for spread in [1e-4,1e-8,1e-12]:
   x=[loc+abs(loc)*spread*t for t in [-2,-1,0,1,3]]
   h=c['held'];env={**h,'inf':math.inf,'Inf':math.inf,'min':min,'max':max,'floor':math.floor,'ceil':math.ceil};valid=True
   for key in ['lower','upper']:
    try:
     v=float(eval(str(m['support'][key]),{'__builtins__':{}},env))
     if key=='lower' and min(x)<v or key=='upper' and max(x)>v:valid=False
    except Exception:pass
   if valid:
    cases.append(dict(index=len(cases),family=m['id'],mask=c['mask'],held=h,sample=f'constant-extra-{loc:g}-{spread:g}',data=x));added+=1
(HERE/'cases.json').write_text(json.dumps(cases));print('distinct-location additions',added,'total cases',len(cases),flush=True)
