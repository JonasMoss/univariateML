"""Check R/WASM observations against independent stationary points."""
import json,sys,mpmath as m
from pathlib import Path
m.mp.dps=90;D=Path(__file__).resolve().parent
cases={r['index']:r for r in json.loads((D/'cases.json').read_text())}
path=D/'stationary.jsonl';refs={r['index']:r for r in map(json.loads,path.read_text().splitlines())}
assert set(refs)==set(cases)
base={r['index']:r for r in map(json.loads,(D/'after-native.jsonl').read_text().splitlines())}
selected=set(json.loads((D/'spot-indices.json').read_text()));outpath=D/'spot-verification.json'
summary=json.loads(outpath.read_text()) if outpath.exists() else {}
for mode in sys.argv[1:]:
 rows=list(map(json.loads,(D/(mode+'.jsonl')).read_text().splitlines()));assert {r['index'] for r in rows}==selected
 own=[];exceptions=[]
 for r in rows:
  i=r['index'];c=cases[i];external=c['family']=='sged' and c['held'].get('xi')==1 and 'nu' not in c['held'];certified=base[i].get('certified')==1
  if r.get('error'):
   if not external:assert base[i]['error'],(mode,i,r)
   continue
  ref=refs[i];target=ref['theta'];keys=list(target);errors={}
  for k,v in target.items():
   if k in c['held']:continue
   t=m.mpf(v);floor=m.mpf(target[keys[1]]) if k==keys[0] else m.mpf(1) if c['family']=='st' and k=='alpha' else m.mpf(0)
   if k==keys[0] and c['family']=='lst' and 'sigma' in c['held']:
    spread=m.mpf(max(c['data']))-m.mpf(min(c['data']))
    if spread>0:floor=spread
   errors[k]=float(abs(m.mpf(float(r['estimates'][k]))-t)/max(abs(t),floor))
  row=dict(index=i,family=c['family'],maximum_scaled_error=max(errors.values(),default=0),components=errors)
  if external or certified:row['scope']='external GED' if external else 'preserved certified';exceptions.append(row)
  else:own.append(row)
 bad=[r for r in own if r['maximum_scaled_error']>1e-10];assert not bad,(mode,bad)
 summary[mode]=dict(cases=len(rows),owned_finite=len(own),maximum_owned_scaled_error=max((r['maximum_scaled_error'] for r in own),default=0),owned=own,preserved_or_external=exceptions)
 print(mode,len(rows),'owned max',summary[mode]['maximum_owned_scaled_error'])
outpath.write_text(json.dumps(summary,indent=2)+'\n')
