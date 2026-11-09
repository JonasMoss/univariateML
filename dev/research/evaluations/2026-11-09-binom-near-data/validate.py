#!/usr/bin/env python3
"""Validate delivered evidence, unchanged goldens and append-only tests."""
from pathlib import Path
import hashlib,json,subprocess,re,math
root=Path.cwd();base=Path(__file__).parent
families=['ztbinom','zmbinom','tbinom','zib']
goldens={}
for family in families:
    path=Path('univariateML-cpp/include/cppunivariate/distributions')/family/'golden.json'
    old=subprocess.check_output(['git','show',f'HEAD:{path}'])
    assert path.read_bytes()==old
    goldens[family]=dict(unchanged=True,sha256=hashlib.sha256(old).hexdigest())
for path in [Path('bindings/js/test.mjs')]+[Path('univariateML-cpp/include/cppunivariate/distributions')/f/'test.cpp' for f in ['tbinom','zib']]+[Path('bindings/r/distributions')/f/'test.R' for f in ['tbinom','zib']]:
    assert path.read_bytes().startswith(subprocess.check_output(['git','show',f'HEAD:{path}'])),path
records=[]
for line in (base/'boundary-after.txt').read_text().splitlines()[:2]:
    tokens=line.split();records.append(dict(zip(tokens[::2],map(float,tokens[1::2]))))
profiles=json.loads((base/'profiles.json').read_text())
matrix=json.loads((base/'final-tests.json').read_text())
assert len(matrix)==10 and all(v['exit_code']==0 for v in matrix.values())
import gzip
for result in matrix.values():
    assert gzip.decompress((base/result['log']).read_bytes()),result['log']
finite=records[0];oracle=profiles[0]['best_in_checked_prefix']
assert finite['status']==0 and finite['size']==oracle['size']==7
prob_error=abs(finite['prob']/float(oracle['prob'])-1)
ll_error=abs(finite['logLik']/float(oracle['logLik'])-1)
assert prob_error<1e-10 and ll_error<1e-10
face=records[1]
assert face['status']==6 and face['comparison']==0 and face['budget']==0
assert face['upper']>=float(profiles[1]['face_logLik'])
assert face['logLik']<=face['upper']
assert finite['cutoff']==907 and face['cutoff']==26593
assert finite['cells']<51200000 and face['cells']<51200000
changed=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
allowed={
    'bindings/common/distributions/tbinom/fit.hpp','bindings/js/test.mjs',
    'bindings/r/distributions/tbinom/test.R','bindings/r/distributions/zib/test.R',
    'univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md',
    'univariateML-cpp/include/cppunivariate/distributions/tbinom/face_certificate.hpp',
    'univariateML-cpp/include/cppunivariate/distributions/tbinom/mle.hpp',
    'univariateML-cpp/include/cppunivariate/distributions/tbinom/test.cpp',
    'univariateML-cpp/include/cppunivariate/distributions/zib/test.cpp',
}
assert all(p in allowed or p=='dev/research/evaluations/2026-11-09-binom-near-report.md' or p.startswith('dev/research/evaluations/2026-11-09-binom-near-data/') for p in changed),changed
assert not any(p.startswith(('univariateML-cpp/include/cppunivariate/core/','univariateML-cpp/include/cppunivariate/special/')) or p.endswith(('/dpqr.hpp','/truth.py','/truth.json','/golden.json')) for p in changed)
manifest={p:hashlib.sha256(Path(p).read_bytes()).hexdigest() for p in changed if not p.startswith('dev/research/evaluations/')}
report=Path('dev/research/evaluations/2026-11-09-binom-near-report.md')
links=[]
if report.exists():
    links=re.findall(r'\[[^\]]*\]\(([^)]+)\)',report.read_text())
    for link in links:
        if '://' not in link:assert (report.parent/link.split('#')[0]).is_file(),link
(base/'golden-integrity.json').write_text(json.dumps(goldens,indent=2)+'\n')
(base/'source-manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
finite={k:(v if math.isfinite(v) else None) for k,v in finite.items()}
(base/'validation.json').write_text(json.dumps(dict(finite=finite,face=face,relative_probability_error=prob_error,relative_loglik_error=ll_error,append_only_tests=True,unchanged_goldens=True,report_links=len(links)),indent=2)+'\n')
print(f'PASS: independent coefficient error {prob_error:.3g}, likelihood error {ll_error:.3g}; four frozen goldens; append-only tests; {len(manifest)} source hashes; {len(links)} report links.')
