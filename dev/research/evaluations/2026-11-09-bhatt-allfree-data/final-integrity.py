"""Read-only git audit plus final source and frozen-input fingerprints."""
from pathlib import Path
import hashlib,json,subprocess
root=Path(__file__).resolve().parent
family='univariateML-cpp/include/cppunivariate/distributions/bhatt/'
allowed={family+s for s in ('README.md','mle.hpp','certificate.hpp','certificate_math.hpp','golden.json','test.cpp')}
allowed.update(('bindings/common/distributions/bhatt/fit.hpp','bindings/r/distributions/bhatt/adapter.cpp','bindings/r/distributions/bhatt/test.R','bindings/js/test.mjs'))
def git(*a):return subprocess.check_output(['git',*a],env=__import__('os').environ|{'GIT_OPTIONAL_LOCKS':'0'})
changed=set(git('diff','--name-only').decode().splitlines())
assert changed==allowed,changed^allowed
old=json.loads(git('show','HEAD:'+family+'golden.json'));new=json.loads(Path(family+'golden.json').read_text())
for k,v in old.items():
    if k not in ('estimates','logLik'):assert new[k]==v,k
assert set(new)-set(old)=={'reference'}
def block(text,name,end):
    start=text.index(name)
    return text[start:text.index(end,start)+len(end)]
restored={}
for path,name,end in (
 ('bindings/r/distributions/bhatt/test.R','test_that("P3 bhatt returns the normal maximum as an attained member"','\n})'),
 (family+'test.cpp','TEST_CASE("bhatt P3 normal boundary is attained with conditional coefficients")','\n}')):
    a=git('show','HEAD:'+path).decode();b=Path(path).read_text()
    assert block(a,name,end)==block(b,name,end)
    restored[name]=True
r=Path('bindings/r/distributions/bhatt/test.R').read_text()
assert 'expect_equal(unname(mlbhatt(c(-10,rep(0,20),10))["a"]),0)' in r
# The complete independent oracle test is restored byte for byte.
name='test_that("'
a=git('show','HEAD:bindings/r/distributions/bhatt/test.R').decode()
face=a.index('expect_match(conditionMessage(f),"mlnorm|mlunif")')
start=a.rfind(name,0,face);end=a.index('\n})',face)+3
assert a[start:end] in r
result={'head':git('rev-parse','HEAD').decode().strip(),'branch':git('branch','--show-current').decode().strip(),
 'frozen_sample_and_all_other_golden_metadata_unchanged':True,
 'restored_named_tests_byte_identical':restored,'original_face_oracle_test_byte_identical':True,
 'shared_test_exception':'Existing held-sigma unresolved-stop blocks now expect a proved normal success. Existing tiny-target positive-held-width stops now expect successful uncertified searches. Iteration and normal-delegate failure tests remain; no tests rewritten to accept new stops.',
 'tracked_changes_within_authorized_scope':True,
 'frozen_data_sha256':hashlib.sha256(json.dumps(new['data'],separators=(',',':')).encode()).hexdigest(),
 'source_sha256':{p:hashlib.sha256(Path(p).read_bytes()).hexdigest() for p in sorted(changed)}}
p=root/'source-integrity.json'
if not (root/'source-integrity-before-review.json').exists():(root/'source-integrity-before-review.json').write_bytes(p.read_bytes())
p.write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
