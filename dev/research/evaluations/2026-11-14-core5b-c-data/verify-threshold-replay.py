"""Verify the metadata replay and inventory the later GPD semantic correction."""
import gzip,json
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3];records=[]
for family in ['gpd','pareto4']:
 p=here/f'native-{family}-before-threshold.jsonl'
 before=p.read_text() if p.exists() else gzip.decompress(p.with_suffix(p.suffix+'.gz').read_bytes()).decode()
 after=(root/'build/snapshots/native'/f'{family}.jsonl').read_text()
 a,b=before.splitlines(),after.splitlines()
 assert len(a)==len(b),family
 changes=[]
 for old,new in zip(a[1:],b[1:]):
  if old==new:continue
  x,y=json.loads(old),json.loads(new)
  # Dossier guards do not affect C++; only the subsequent finite-collapse
  # outcome correction may change a native result in this replay.
  u,v=x['result'],y['result']
  assert family=='gpd' and u['kind']=='limit' and v['kind']=='nomle' and v['reason']=='unattained',(family,x,y)
  assert u['core_status']==v['core_status'] and u['limit_loglik']==v['limit_loglik'],(x,y)
  changes.append(dict(before=x,after=y))
 records.append(dict(family=family,cases=len(a)-1,before_tag=a[0],after_tag=b[0],result_changes=len(changes),changes=changes))
(here/'threshold-replay.json').write_text(json.dumps(records,indent=2)+'\n')
print('Threshold replay:',[(r['family'],r['cases'],r['result_changes']) for r in records])
