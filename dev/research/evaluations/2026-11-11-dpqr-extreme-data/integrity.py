"""Check the historical numerical prefix and collect exact table-size changes."""
import json,subprocess,os
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
D=ROOT/'univariateML-cpp/include/cppunivariate/distributions'
if os.environ.get('UMVL_AUDIT_SKIP_BASELINE')=='1':
 print('Git-baseline prefix and forbidden-file checks are reserved for the workstation checkout')
 raise SystemExit(0)
baseline=json.loads((HERE/'baseline-sizes.json').read_text());records=[]
for ident,oldsize in baseline['families'].items():
 relative=Path('univariateML-cpp/include/cppunivariate/distributions')/ident/'truth.json'
 old=json.loads(subprocess.check_output(['git','show',baseline['commit']+':'+str(relative)],cwd=ROOT))
 measured=HERE/'measured-tables'/(ident+'.json')
 path=measured if measured.exists() else ROOT/relative
 new=json.loads(path.read_text())
 for key in ('parameters','values','quantiles','normalization','sampler_points'):
  assert new[key][:len(old[key])]==old[key],(ident,key)
 for key in ('excluded_values','excluded_quantiles','excluded_parameter_points'):
  assert new.get(key,[])[:len(old.get(key,[]))]==old.get(key,[]),(ident,key)
 records.append(dict(family=ident,before=oldsize,after=dict(bytes=path.stat().st_size,points=len(new['parameters']),values=len(new['values']),quantiles=len(new['quantiles'])),
  excluded_values=len(new.get('excluded_values',[])),excluded_quantiles=len(new.get('excluded_quantiles',[])),oracle_gaps=new.get('extreme_reference_gaps',[])))
changed=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
for p in changed:
 assert not any(p.endswith('/'+s) for s in ('mle.hpp','golden.json')) and not any('/'+s+'/' in p for s in ('frechet','invweibull')),p
 assert p not in ('bindings/r/NEWS.md','dev/STATUS.md','dev/KNOWN_ISSUES.md') and not p.startswith('dev/roadmap/'),p
out=dict(baseline=baseline['commit'],historical_prefix_verified=True,families=records)
(HERE/'final-sizes.json').write_text(json.dumps(out,indent=2)+'\n')
print('historical numeric prefix intact for',len(records),'tables; forbidden files unchanged')
for key in ('bytes','points','values','quantiles'):
 print(key,sum(r['before'][key] for r in records),sum(r['after'][key] for r in records))
