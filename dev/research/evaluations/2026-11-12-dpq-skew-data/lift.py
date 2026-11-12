"""Reactivate only declarations; compare immutable rows against git baseline."""
import json,subprocess,collections
from pathlib import Path
H=Path(__file__).resolve().parent;D=Path('univariateML-cpp/include/cppunivariate/distributions')
def inventory(d):
 ps=d['parameters'][:];out=[]
 for k in ('values','quantiles'):
  for r in d[k]+[r['row'] for r in d.get('excluded_'+k,[])]:out.append([k,ps[r[0]],r[1:]])
 for p in d.get('excluded_parameter_points',[]):
  for k in ('values','quantiles'):
   for r in p[k]:out.append([k,p['parameters'],r[1:]])
 return collections.Counter(json.dumps(r,sort_keys=True) for r in out)
receipts=[]
for f in ['ged','sged','sstd','st']:
 p=D/f/'truth.json';before=json.loads(subprocess.check_output(['git','show','HEAD:'+str(p)],text=True));d=json.loads(p.read_text());counts={}
 for k in ('values','quantiles'):
  ex=d.pop('excluded_'+k,[]);d[k].extend(r['row'] for r in ex);counts[k]=len(ex)
 for pt in d.pop('excluded_parameter_points',[]):
  idx=len(d['parameters']);d['parameters'].append(pt['parameters'])
  for n in pt['normalization']:d['normalization'].append(dict(n,point=idx))
  for k in ('values','quantiles'):d[k].extend([idx,*r[1:]] for r in pt[k])
 assert inventory(before)==inventory(d),f
 # Keep formatting consistent with the fixture writers.
 p.write_text(json.dumps(d,indent=2,ensure_ascii=False)+'\n')
 receipts.append(dict(family=f,lifted_rows=counts,parameter_points_before=len(before['parameters']),parameter_points_after=len(d['parameters']),reference_rows_unchanged=True))
(H/'reference-preservation.json').write_text(json.dumps(receipts,indent=2)+'\n')
