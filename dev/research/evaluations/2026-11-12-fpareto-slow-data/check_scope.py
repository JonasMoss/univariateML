"""Read-only scope, appended-test and frozen-source checks."""
import hashlib,json,subprocess
from pathlib import Path
p=Path(__file__).resolve().parent;root=p.parents[3];base='bb46078757dad3538bfb263086d4e72cde163330'
allowed={f'univariateML-cpp/include/cppunivariate/distributions/{family}/{name}' for family in ['fpareto','gb2'] for name in ['README.md','mle.hpp','test.cpp']}
changed=set(subprocess.check_output(['git','diff','--name-only'],cwd=root,text=True).splitlines());assert changed==allowed,changed
for family in ['fpareto','gb2']:
 rel=f'univariateML-cpp/include/cppunivariate/distributions/{family}/test.cpp';old=subprocess.check_output(['git','show',base+':'+rel],cwd=root);assert (root/rel).read_bytes().startswith(old),rel
record=json.loads((p/'comparison.json').read_text())
for rel,digest in record['files'].items():assert hashlib.sha256((root/rel).read_bytes()).hexdigest()==digest,rel
out=dict(changed=sorted(changed),appended_tests_only=True,fit_sources_unchanged_since_replay=True,no_forbidden_tracked_edits=True)
(p/'scope-check.json').write_text(json.dumps(out,indent=2)+'\n');print(json.dumps(out,indent=2))
