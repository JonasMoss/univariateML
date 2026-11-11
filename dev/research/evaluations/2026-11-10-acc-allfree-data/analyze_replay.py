import json,math
from pathlib import Path
D=Path(__file__).resolve().parent
root=D.parents[3]
a=json.loads((D/'audit-cases.json').read_text());fits={phase:{r['index']:r for r in map(json.loads,(D/f'replay-{phase}.jsonl').read_text().splitlines())} for phase in ['before','after']}
assert all(len(f)==len(a) for f in fits.values())
rows=[]
for entry in a:
 c=entry['case'];ref=entry['reference'];names=list(json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{c["family"]}/golden.json').read_text())['estimates']);target=ref.get('refined_theta',ref.get('better_theta'))
 row=dict(index=c['index'],family=c['family'],mask=c['mask'],sample=c['sample'],allfree=c['mask']==0)
 for phase,f in fits.items():
  r=f[c['index']];row[phase+'_error']=r['error']
  if not r['error'] and target:
   row[phase+'_relative']=max(abs(r['estimates'][k]-float(v))/(abs(float(v)) or 1) for k,v in zip(names,target) if k not in c['held'])
 rows.append(row)
(D/'replay-analysis.json').write_text(json.dumps(rows,indent=2)+'\n')
for id in ['slash','sstd','sged','st','bnbinom','gb2']:
 r=[v for v in rows if v['family']==id and v['allfree']];print(id,'allfree',len(r));print(r)
summary={}
for ident in ['slash','sstd','sged','st','bnbinom','gb2']:
 r=[v for v in rows if v['family']==ident]
 summary[ident]={
  'cases':len(r),
  'allfree':sum(v['allfree'] for v in r),
  'maximum_allfree_before':max((v.get('before_relative',0) for v in r if v['allfree']),default=None),
  'maximum_allfree_after':max((v.get('after_relative',0) for v in r if v['allfree']),default=None),
  'remaining_held_misses':[v['index'] for v in r if not v['allfree'] and v.get('after_relative',0)>1e-10],
  'existing_stops':[{'index':v['index'],'error':v['after_error']} for v in r if v['after_error']],
  'new_stops':[v['index'] for v in r if not v['before_error'] and v['after_error']]}
(D/'replay-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
assert all(not v['after_error'] and v['after_relative']<=1e-10 for v in rows if v['allfree'])
assert not any(v['new_stops'] for v in summary.values())
