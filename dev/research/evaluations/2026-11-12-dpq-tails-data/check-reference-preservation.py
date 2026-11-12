"""Verify that lifting exclusions changes no independent reference value."""
import collections,json,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3];results=[]
for f in ['beta','betapr','fpareto','gb2']:
 path=Path('univariateML-cpp/include/cppunivariate/distributions')/f/'truth.json'
 before=json.loads(subprocess.check_output(['git','show','217ab8b5:'+str(path)],cwd=root,text=True));after=json.loads((root/path).read_text())
 def rows(t):return collections.Counter(json.dumps(r) for r in t['values']+[v['row'] for v in t.get('excluded_values',[])])
 assert rows(before)==rows(after)
 for field in ['parameters','quantiles','normalization','sampler_points']:assert before[field]==after[field]
 results.append(dict(family=f,restored=len(before.get('excluded_values',[])),references_preserved=True))
(here/'reference-preservation.json').write_text(json.dumps(results,indent=2)+'\n');print(results)
