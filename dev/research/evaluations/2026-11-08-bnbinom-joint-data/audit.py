"""Read-only scope, frozen-fixture, append-only and source-identity checks."""
import hashlib,json,subprocess
from pathlib import Path
base='univariateML-cpp/include/cppunivariate/distributions/bnbinom/'
for name in ('golden.json','dpqr.hpp','truth.py','truth.json'):
    path=base+name
    assert Path(path).read_bytes()==subprocess.check_output(['git','show','HEAD:'+path]),path
for path in (base+'test.cpp','bindings/r/distributions/bnbinom/test.R','bindings/js/test.mjs'):
    assert Path(path).read_bytes().startswith(subprocess.check_output(['git','show','HEAD:'+path])),path
allowed={base+n for n in ('mle.hpp','pair_certificate.hpp','README.md','test.cpp')}
allowed.update(('bindings/common/distributions/bnbinom/fit.hpp','bindings/r/distributions/bnbinom/test.R','bindings/js/test.mjs'))
changed=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
assert set(changed)<=allowed,set(changed)-allowed
untracked=subprocess.check_output(['git','ls-files','--others','--exclude-standard'],text=True).splitlines()
for path in untracked:
    assert path==base+'pair_certificate.hpp' or path.startswith('dev/research/evaluations/2026-11-08-bnbinom-joint-data/') or path=='dev/research/evaluations/2026-11-08-bnbinom-joint-report.md',path
source=Path(__file__).with_name('source.json')
if source.exists():
    for path,digest in json.loads(source.read_text())['sha256'].items():
        assert hashlib.sha256(Path(path).read_bytes()).hexdigest()==digest,path
subprocess.run(['git','diff','--check'],check=True)
print('PASS: settled files and golden byte-identical; test files append-only; tracked scope and source identity match; whitespace clean.')
