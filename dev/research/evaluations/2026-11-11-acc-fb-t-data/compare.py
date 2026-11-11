"""Compare every free public coefficient with independent 90-digit roots."""
from pathlib import Path
import json,mpmath as m
D=Path(__file__).resolve().parent;m.mp.dps=90
cases={r['index']:r for r in json.loads((D/'cases.json').read_text())}
refs={r['index']:r for r in map(json.loads,(D/'stationary.jsonl').read_text().splitlines())}
summary={};details=[]
for mode in ['before-native','before-double','after-native','after-double','after-r','after-wasm']:
 path=D/(mode+'.jsonl')
 if not path.exists():continue
 rows=list(map(json.loads,path.read_text().splitlines()));routes={}
 for r in rows:
  i=r['index'];c=cases[i];ref=refs.get(i,{});route=c['family']+':'+','.join(c['held'])
  group=routes.setdefault(route,dict(count=0,stops=[],maximum_scaled_error=0,worst=None,certified_counts={}))
  group['count']+=1;flag=str(r.get('certified'));group['certified_counts'][flag]=group['certified_counts'].get(flag,0)+1
  if r['error']:group['stops'].append(i);continue
  if 'theta' not in ref:continue
  target=ref['theta'];keys=list(target);location=keys[0];scale=keys[1];errors={}
  for k,v in target.items():
   if k in c['held']:continue
   t=m.mpf(v);floor=m.mpf(target[scale]) if k==location else m.mpf(1) if c['family']=='st' and k=='alpha' else m.mpf(0)
   if k==location and c['family']=='lst' and 'sigma' in c['held']:
    spread=m.mpf(max(c['data']))-m.mpf(min(c['data']))
    if spread>0:floor=spread
   errors[k]=float(abs(m.mpf(r['estimates'][k])-t)/max(abs(t),floor))
  worst=max(errors.values(),default=0)
  details.append(dict(mode=mode,index=i,route=route,components=errors,maximum_scaled_error=worst,certified=r.get('certified')))
  if worst>group['maximum_scaled_error']:group['maximum_scaled_error']=worst;group['worst']=i
 summary[mode]=routes
(D/'comparison.json').write_text(json.dumps(summary,indent=2)+'\n');(D/'coefficient-errors.json').write_text(json.dumps(details,indent=2)+'\n')
for mode,routes in summary.items():
 print(mode,'max',max(v['maximum_scaled_error'] for v in routes.values()),'stops',sum(len(v['stops']) for v in routes.values()))
 for route,row in routes.items():
  if row['maximum_scaled_error']>1e-10:print(' ',route,row['maximum_scaled_error'],row['worst'])
