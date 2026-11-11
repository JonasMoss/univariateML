"""Final source/fixture and no-regression audit; run after final suites."""
from pathlib import Path
import csv,json,hashlib,subprocess
D=Path('dev/research/evaluations/2026-11-10-gev-held2-data')
manifest=json.loads((D/'combination-sources.json').read_text())
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
checks={}
checks['frozen_sources_match']=all(sha(p)==s for p,s in manifest['source_sha256'].items())
changes=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
protected=['/dpqr.hpp','/golden.json','/truth.py','/truth.json','/special/','bindings/r/NEWS.md','dev/STATUS.md','dev/KNOWN_ISSUES.md','dev/roadmap/','bindings/r/scripts/codegen','/core/','bindings/common/','bindings/js/']
checks['protected_files_unchanged']=not any(any(t in p for t in protected) for p in changes if p!='bindings/js/fit-test.mjs')
p=Path('bindings/js/fit-test.mjs')
checks['js_fit_tests_append_only']=p.read_bytes().startswith(subprocess.check_output(['git','show','HEAD:'+str(p)]))
for family in ('gev','frechet'):
 p=Path('univariateML-cpp/include/cppunivariate/distributions')/family/'test.cpp'
 original=subprocess.check_output(['git','show','HEAD:'+str(p)])
 checks[family+'_tests_append_only']=p.read_bytes().startswith(original)
for maxit in (100,1000):
 before={(r['sample_id'],r['family'],r['hold']):r for r in csv.DictReader((D/f'study-local-{maxit}.csv').open())}
 after={(r['sample_id'],r['family'],r['hold']):r for r in csv.DictReader((D/f'combination-policy-{maxit}.csv').open())}
 checks[f'grid_{maxit}_same_rows']=set(before)==set(after) and len(after)==160
 checks[f'grid_{maxit}_no_status_change']=all(before[k]['status']==r['status'] for k,r in after.items())
 checks[f'grid_{maxit}_no_certificate_lost']=all(int(before[k]['certified'])<=int(r['certified']) for k,r in after.items())
 if maxit==100:
  counts={(fam,hold):sum(int(r['certified']) for r in after.values() if (r['family'],r['hold'])==(fam,hold)) for fam,hold in [('gev','mu'),('gev','sigma'),('frechet','sigma')]}
  checks['scope_targets_met']=counts[('gev','mu')]>=44 and counts[('gev','sigma')]>=54 and counts[('frechet','sigma')]==40
 refs=[json.loads(l) for l in (D/'combination-policy-reference.jsonl').read_text().splitlines()]
 expected={(k[0],k[1],k[2]) for k,r in after.items() if r['certified']=='0'}
 actual={(str(r['sample_id']),r['family'],r['hold']) for r in refs if r['source']==f'combination-policy-roots-{maxit}.jsonl'}
 checks[f'grid_{maxit}_all_uncertified_checked_at_70_digits']=expected==actual
checks['independent_roots_meet_tolerance']=all(r['max_error']<=1e-10 for r in refs)
for mode in ('native','double'):
 refs=[json.loads(l) for l in (D/f'combination-seam-reference-{mode}.jsonl').read_text().splitlines()]
 checks[f'seam_{mode}_four_fits_meet_tolerance']=len(refs)==4 and all(r['max_error']<=1e-10 for r in refs)
for mode in ('native','double'):
 for kind in ('held-reference','held-public-reference'):
  refs=[json.loads(l) for l in (D/f'combination-{kind}-{mode}.jsonl').read_text().splitlines()]
  checks[f'{kind}_{mode}_four_fits_meet_tolerance']=len(refs)==4 and all(r['max_error']<=1e-10 for r in refs)
for family in ('gev','frechet'):
 for p in (Path('univariateML-cpp/include/cppunivariate/distributions')/family).glob('*.hpp'):
  generated=Path('univariateML-r/inst/include/cppunivariate/distributions')/family/p.name
  checks['generated_'+family+'_'+p.name]=generated.exists() and sha(p)==sha(generated)
checks['new_header_present_in_generated_package']=Path('univariateML-r/inst/include/cppunivariate/distributions/gev/accuracy.hpp').exists()
large=list(csv.DictReader((D/'combination-scaling-growth.csv').open()))
checks['scaling_twelve_samples_certify_at_default']=len(large)==12 and all(r['status']=='0' and r['certified']=='1' and r['iterlim']=='100' for r in large)
result={'baseline':manifest['baseline'],'checks':checks,'changed_tracked_files':changes,'source_sha256':manifest['source_sha256']}
(D/'combination-audit.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(checks,indent=2))
if not all(checks.values()):raise SystemExit(1)
