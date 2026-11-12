"""Lift closed exclusions without changing the multiset of reference numbers."""
import collections,json,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
D=ROOT/'univariateML-cpp/include/cppunivariate/distributions'
BASE='217ab8b533cac25ccc8eba8d66eccde1b376f0b4'
def payload(t):
 rows=[]
 for category in ('values','quantiles'):
  for r in t[category]+[v['row'] for v in t.get('excluded_'+category,[])]:
   rows.append(json.dumps([category,t['parameters'][r[0]],*r[1:]]))
  for p in t.get('excluded_parameter_points',[]):
   for r in p[category]:rows.append(json.dumps([category,p['parameters'],*r[1:]]))
 return collections.Counter(rows)
receipts=[]
for f in ['bnbinom','cmp','lgser','pig','skellam']:
 p=D/f/'truth.json';t=json.loads(p.read_text())
 old=json.loads(subprocess.check_output(['git','show',BASE+':'+str(p.relative_to(ROOT))],text=True))
 for member in t.pop('excluded_parameter_points',[]):
  index=len(t['parameters']);t['parameters'].append(member['parameters'])
  for category in ('values','quantiles'):
   t[category].extend([index,*r[1:]] for r in member[category])
  t['normalization'].extend(dict(row,point=index) for row in member['normalization'])
 for category in ('values','quantiles'):
  t[category].extend(r['row'] for r in t.pop('excluded_'+category,[]))
 assert payload(t)==payload(old),f
 for key in ('sampler_points','historical_sampler_points'):
  assert t.get(key)==old.get(key)
 p.write_text(json.dumps(t,indent=2,allow_nan=False)+'\n')
 receipts.append(dict(family=f,numeric_payload_preserved=True,values=len(t['values']),quantiles=len(t['quantiles'])))
(Path(__file__).parent/'preservation.json').write_text(json.dumps(receipts,indent=2)+'\n')
