import json,gzip,collections
from pathlib import Path
D=Path(__file__).resolve().parent
cs={c['index']:c for c in json.loads((D/'cases.json').read_text())}
summary={}
for mode in ('native','double'):
 b={r['index']:r for r in json.loads((D/f'before-{mode}-checked.json').read_text())};a={r['index']:r for r in json.loads((D/f'after-{mode}-checked.json').read_text())}
 changes=[];new=[];stops=[];goldens=[];groups={}
 for f in ('sged','sstd','snorm'):
  group={}
  for phase,rows in [('before',b),('after',a)]:
   v=[r for i,r in rows.items() if r['family']==f and i<100000];ok=[r for r in v if not r['error']]
   group[phase]=dict(datasets=len(v),successes=len(ok),certified=sum(r['certified']==1 for r in ok),fitter_misses=[r['index'] for r in ok if r['fitter_error']>1e-10],dpqr_misses=[r['index'] for r in ok if r['dpqr_error'] is None or r['dpqr_error']>1e-10],max_fitter_error=max(r['fitter_error'] for r in ok),max_dpqr_error=max(r['dpqr_error'] or 0 for r in ok),stops=dict(collections.Counter(r['error'] for r in v if r['error'])))
  groups[f]=group
 for i,r in a.items():
  if r['error'] and not b[i]['error']:stops.append(i)
  if not r['error'] and b[i]['error']:new.append(i)
  if not r['error'] and not b[i]['error'] and r['parameters']!=b[i]['parameters']:changes.append(i)
  if 200000<=i<200003:goldens.append(dict(index=i,unchanged=all(r[k]==b[i][k] for k in ['parameters','logLik','certified','error'])))
 assert not stops,stops
 assert all(r['unchanged'] for r in goldens),goldens
 # Attribute the old seam failures against these fresh refits, including stops.
 old=json.loads(gzip.decompress((D.parent/'2026-11-11-dpqr-extreme-data/seam-final.json.gz').read_bytes()))
 oldrows=old['mismatches'];attribution={}
 for f in ('sged','sstd','snorm'):
  v=[r for r in oldrows if r['family']==f and 'fitter' in r.get('attribution','') and r['index']>=0]
  attribution[f]=dict(archived_fitter_errors=len(v),now_success=[r['index'] for r in v if not a[r['index']]['error']],now_stopped=[dict(index=r['index'],reason=a[r['index']]['error']) for r in v if a[r['index']]['error']])
 summary[mode]=dict(groups=groups,new_stops=stops,new_successes=new,changed_parameters=changes,goldens=goldens,old_seam_attribution=attribution)
roots=json.loads((D/'all-certified-roots.json').read_text());movements=[]
for mode in ('native','double'):
 b={r['index']:r for r in roots['rows'] if r['phase']=='before' and r['mode']==mode};a={r['index']:r for r in roots['rows'] if r['phase']=='after' and r['mode']==mode}
 for i,r in a.items():
  if r['error'] or b[i]['error']:continue
  for k,v in r['components'].items():
   old=b[i]['components'][k]
   if v>old:movements.append(dict(mode=mode,index=i,component=k,before=old,after=v))
summary['roots']=roots['summary'];summary['component_error_increases']=movements
(D/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
