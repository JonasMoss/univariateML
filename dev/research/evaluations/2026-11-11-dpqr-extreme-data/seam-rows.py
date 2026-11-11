"""Resolve aggregate seam errors into exact-input scalar log-density errors."""
import json,math,sys,collections
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
sys.path.insert(0,str(HERE));import audit
from quality.truth import read
from seam_reference import reference
s=json.loads((HERE/('seam-'+(sys.argv[1] if len(sys.argv)>1 else 'final')+'.json')).read_text())
records=[r for r in s['mismatches'] if r['attribution'] in ('dpqr','both')]
jobs=[(r['family'],0,x,True,True,r['parameters']) for r in records for x in r['data']]
actual=iter(audit.evaluate(jobs));failed=[];passing=0
for r in records:
 meta,_=read(audit.DISTS/r['family']);theta=dict(zip([p['name'] for p in meta['params']],r['parameters']))
 maxexp=max([abs(math.log10(abs(v))) for v in r['parameters'] if v and math.isfinite(v)]+[0]);mp.mp.dps=min(1600,max(100,int(4*maxexp)+100))
 for x in r['data']:
  got=next(actual);want=reference(audit.DISTS/r['family'],meta,theta,[x]);error=audit.error(got,want)
  if error>1e-10:failed.append(dict(family=r['family'],function='d',input=x,parameters=r['parameters'],actual=got,expected=want,error=error,source=r['source'],case_index=r['index']))
  else:passing+=1
(HERE/('seam-row-'+('after' if len(sys.argv)>1 else 'before')+'-failures.json')).write_text(json.dumps(dict(checks=len(jobs),passing=passing,failures=failed),indent=2)+'\n')
print('seam scalar rows',len(jobs),'failures',len(failed),collections.Counter(r['family'] for r in failed),flush=True)
