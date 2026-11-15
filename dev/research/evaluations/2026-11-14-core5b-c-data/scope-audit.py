"""Check the final patch boundary using read-only Git operations."""
import json,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3]
F=set('betapr burr cmp dnorm emg foldnorm gb2 gengamma gpd hnorm hyper invgamma invpareto kumar lnorm lomax nhyper pareto pareto4 rayleigh skellam snorm std triang yulesimon'.split())
changed=subprocess.check_output(['git','diff','--name-only'],cwd=root,text=True).splitlines()
for name in changed:
 p=Path(name);s=p.parts
 if name in ['dev/validation/strict-known.json','dev/validation/capabilities.csv','bindings/r/tests/testthat/test-outcomes.R','bindings/js/stop-test.mjs','bindings/js/test.mjs']:continue
 if s[:2]==('dev','research'):continue
 if s[:4]==('univariateML-cpp','include','cppunivariate','distributions'):
  assert s[4] in F,name
  assert s[-1] not in ['dpqr.hpp','golden.json','truth.py','truth.json'],name
 elif s[:3]==('bindings','common','distributions'):assert s[3] in F,name
 elif s[:3]==('bindings','r','distributions'):assert s[3] in F,name
 else:raise AssertionError(name)
subprocess.run(['git','diff','--check'],cwd=root,check=True)
record=dict(baseline=subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),changed_tracked_files=changed,
 protected_files_changed=False,goldens_or_dpqr_changed=False,unassigned_family_source_changed=False)
(here/'scope-audit.json').write_text(json.dumps(record,indent=2)+'\n')
print('Patch scope and whitespace checks passed:',len(changed),'tracked files.')
