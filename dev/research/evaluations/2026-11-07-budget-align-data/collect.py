"""Collect completed foreground runs; refuses missing or failed final runs."""
from pathlib import Path
import hashlib,json,subprocess,collections
base=Path(__file__).resolve().parent
names=['cpp-dev-final11','cpp-full-final11','cpp-no-long-double-final11','r-dev-final11','r-full-final11','js-build-final11','js-test-final11','site-check-final11','gen-final12','relations-final12','environment-final12']
runs={name:json.loads((base/(name+'.json')).read_text()) for name in names}
assert all(r['returncode']==0 for r in runs.values())
tracked=[Path(p) for p in subprocess.check_output(['git','diff','--name-only'],text=True).splitlines() if not p.endswith('.png')]
tracked += [Path('univariateML-cpp/include/cppunivariate/core/work_budget.hpp')]
manifest=''.join(f'{hashlib.sha256(p.read_bytes()).hexdigest()}  {p}\n' for p in sorted(tracked) if p.is_file())
(base/'source-sha256.txt').write_text(manifest)
forbidden=[str(p) for p in tracked if '/special/' in str(p) or p.name in ['dpqr.hpp','golden.json','truth.py','truth.json','NEWS.md','STATUS.md','KNOWN_ISSUES.md'] or '/roadmap/' in str(p) or 'codegen' in str(p)]
assert not forbidden,forbidden
assert 'dev/design/work-budgets.md' not in [str(p) for p in tracked]
# Documentation-only cap wording was corrected after the numerical source froze.
# Retain each run's actual full source fingerprint instead of equating it to
# the post-validation documentation fingerprint.
headers=sorted([*Path('univariateML-cpp/include').rglob('*.hpp'),*Path('bindings/common').rglob('*.hpp')])
header_manifest=''.join(f'{hashlib.sha256(p.read_bytes()).hexdigest()}  {p}\n' for p in headers)
header_sha=hashlib.sha256(header_manifest.encode()).hexdigest()
assert header_sha==(base/'final-header-sha256.txt').read_text().strip()
assert all(r.get('source_headers_sha256',header_sha)==header_sha for r in runs.values())
nb=json.loads((base/'nbfam-final9.json').read_text())
stress=json.loads((base/'stress-combined.json').read_text()); rows=stress['records']
result=dict(source_headers_sha256=header_sha,revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),source_sha256=hashlib.sha256(manifest.encode()).hexdigest(),runs=runs,nbfam=dict(cases=nb['cases'],status_counts=dict(collections.Counter(r['status'] for r in nb['results'])),actual_budget_stops=sum(r['budget_exhausted'] for r in nb['results']),precision=nb['precision'],max_errors=nb['max_errors'],interval_checks=nb['interval_checks'],machine='workstation'),stress=dict(cases=len(rows),families=len(stress['summary']),censored=sum(bool(r.get('censored')) for r in rows),summary_file='stress-summary.csv'),forbidden_changes=forbidden)
(base/'validation.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
