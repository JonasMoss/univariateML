#!/usr/bin/env python3
"""Final frozen-input, imported-reference and current-oracle checks; run in one slot."""
import json,subprocess,sys
from decimal import Decimal,localcontext
from pathlib import Path
import mpmath as mp
out=Path(__file__).resolve().parent
root=out.parents[3]
sys.path.insert(0,str(root/'dev/validation'))
from quality.reference import DISTS,rescore_oracle
history=json.loads((out/'checks.json').read_text())
checks={r['name']:r for r in history}
assert all(checks[k]['exit_code']==0 for k in ('cpp-dev','r-dev','js-build','js-test','cpp-full','r-full','cpp-no-long-double'))
assert max(i for i,r in enumerate(history) if r['name']=='js-test') > max(i for i,r in enumerate(history) if r['name']=='cpp-no-long-double')
dossiers=json.loads((out/'dossier-evidence-checks.json').read_text())
assert len(dossiers)==48 and all(r['matches_current_dossier'] for r in dossiers)
# Preserve the imported values so subsequent audits need no other worktree.
baseline_path=out/'imported-reference-values.json'
if not baseline_path.exists():
 old=root.parent/'golden-ref'
 baseline={}
 for p in sorted(DISTS.glob('*/golden.json')):
  if p.parent.name=='bhatt':continue
  ref=json.loads((old/p.relative_to(root)).read_text())['reference']
  baseline[p.parent.name]={k:ref[k] for k in ('estimates','logLik','input_sha256')}
 baseline_path.write_text(json.dumps(baseline,indent=2)+'\n')
baseline=json.loads(baseline_path.read_text())
quality={r['id']:r for r in json.loads((out/'quality.json').read_text())}
result={'imported_reference_checks':[],'current_oracle_rescoring':{},'integer_comparisons':{}}
cases=json.loads((root/'build/js/generated/fit-cases.json').read_text())
b=next(c for c in cases if c['id']=='bhatt')
assert b['tol']==1e-10 and b['ll_tol']==1e-7 and b['ll_scale']==1
assert all(v==5e-7 for v in b['tolerances'].values())
assert all(v==1 for v in b['tolerance_scales'].values())
result['bhatt_js_policy_unchanged']={k:b[k] for k in ('tol','tolerances','tolerance_scales','ll_tol','ll_scale')}
p=DISTS/'bhatt/golden.json'
assert p.read_bytes()==subprocess.check_output(['git','show','HEAD:'+str(p.relative_to(root))],cwd=root)
for p in sorted(DISTS.glob('*/golden.json')):
 id=p.parent.name
 if id=='bhatt':continue
 ref=json.loads(p.read_text())['reference'];old=baseline[id]
 assert ref['input_sha256']==old['input_sha256'],id
 a={**ref['estimates'],'logLik':ref['logLik']};b={**old['estimates'],'logLik':old['logLik']}
 with localcontext() as ctx:
  ctx.prec=60
  errors={}
  for k,v in a.items():
   if v==b[k]:errors[k]=0.;continue
   x,y=Decimal(v),Decimal(b[k]);assert x.is_finite() and y.is_finite(),(id,k)
   errors[k]=float(abs(x-y)/(abs(y) or Decimal(1)))
  assert max(errors.values())<1e-22,(id,errors)
 result['imported_reference_checks'].append({'id':id,'all_value_strings_identical':a==b,'max_relative_difference':max(errors.values())})
 oracle=quality[id]['oracle']
 if oracle.get('gap',0)>1e-12*(1+abs(float(ref['logLik']))):
  checked=rescore_oracle(id,ref,oracle)
  with mp.workdps(60):assert mp.mpf(checked['gap'])<=mp.mpf(checked['comparison_margin']),(id,checked)
  result['current_oracle_rescoring'][id]=checked
 integer=ref.get('integer_enumeration')
 if integer:
  with mp.workdps(60):
   margin=mp.mpf('1e-40')*(1+abs(mp.mpf(ref['logLik'])))
   gaps={k:integer[k] for k in ('runner_up_gap','tail_gap') if integer.get(k) is not None}
   assert all(mp.mpf(v)>margin for v in gaps.values()),(id,gaps)
   result['integer_comparisons'][id]={'precision_digits':50,'practical_comparison_margin':mp.nstr(margin,3),'gaps':gaps}
assert len(result['imported_reference_checks'])==98
(out/'final-audit.json').write_text(json.dumps(result,indent=2)+'\n')
# Re-run the strict five-mode calibration and frozen JSON byte/value checks.
subprocess.run([sys.executable,str(out/'audit.py')],cwd=root,check=True)
subprocess.run(['git','diff','--check'],cwd=root,check=True)
print('Final audit passed:',len(result['imported_reference_checks']),'references; oracle rescoring',list(result['current_oracle_rescoring']),flush=True)
