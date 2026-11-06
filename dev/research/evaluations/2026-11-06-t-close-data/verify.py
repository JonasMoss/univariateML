"""Retained independent roots, endpoints, goldens and multimodal winners."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
here=Path(__file__).resolve().parent
rows=json.loads((here/'replay.json').read_text())
refs=json.loads((here.parent/'2026-11-05-t-root-data/independent.json').read_text())
mm=json.loads((here.parent/'2026-11-04-t-closure-data/multimodality.json').read_text())
checks=[]
for row in rows:
 if row['status'] not in (0,1,3):continue
 box=[list(map(mp.mpf,row['box'][2*j:2*j+2])) for j in range(3)]
 if row['name'] in mm:
  ref=max(mm[row['name']]['stationary_candidates'],key=lambda p:mp.mpf(p['ll']))
 elif row['status']==0 and row['name']!='quantile-4000':
  ref=next(r['coefficients'] for r in refs['roots_checked'] if (r['name'],r['family'])==(row['name'],row['family']))
 elif row['status'] in (1,3):
  r=next(r for r in refs['endpoint_roots_checked'] if (r['name'],r['family'])==(row['name'],row['family']))
  ref=dict(mu=r['mu'],scale=r['ordinary_scale'],nu=r['nu'])
 elif row['name']=='quantile-4000' and (here/'quantile-root.json').exists():
  ref=json.loads((here/'quantile-root.json').read_text())['coefficients']
 else:continue
 for j,name in enumerate(['mu','scale','nu']):assert box[j][0]<mp.mpf(ref[name])<box[j][1],(row,name,ref[name])
 checks.append(dict(name=row['name'],family=row['family'],independent_root_inside=True))
 if row['name']==row['family']+'-golden':
  golden=json.loads((here.parents[3]/f'univariateML-cpp/include/cppunivariate/distributions/{row["family"]}/golden.json').read_text())['estimates']
  if row['family']=='std':
   sl,sh=box[1];vl,vh=box[2];box[1]=[sl*mp.sqrt(vh/(vh-2)),sh*mp.sqrt(vl/(vl-2))]
  names=['mu','sigma','df'] if row['family']=='lst' else ['mean','sd','nu']
  err=max(abs(v-mp.mpf(golden[n]))/abs(mp.mpf(golden[n])) for b,n in zip(box,names) for v in b)
  assert err<mp.mpf('3e-5'),(row,err)
  checks[-1]['whole_box_golden_relative_error']=str(err)
(here/'independent.json').write_text(json.dumps(checks,indent=2)+'\n')
print('Passed independent retained root/endpoint/golden checks:',len(checks))
