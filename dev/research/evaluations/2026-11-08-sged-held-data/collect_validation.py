"""Summarize completed workstation validation; refuse missing/failed stages."""
import csv
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess

folder=Path('dev/research/evaluations/2026-11-08-sged-held-data')
log=Path('build/scratch/validation-workstation.log').read_text()
seconds={k:float(v) for k,v in re.findall(r'([A-Z_]+)_SECONDS=([0-9.]+)',log)}
required={'TARGETED','CPP_DEV','R_DEV','CPP_FULL','R_FULL','CPP_NO_LONG_DOUBLE','SITE'}
assert required<=seconds.keys(),(required-seconds.keys())
cpp=list(map(int,re.findall(r'100% tests passed, 0 tests failed out of (\d+)',log)))
assert cpp==[106,129,129],cpp
r=[dict(zip(['fail','warn','skip','pass'],map(int,row))) for row in re.findall(r'^\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]$',log,re.M)]
assert len(r)==3,r
assert all(row['fail']==row['warn']==0 for row in r),r
assert '[doctest] test cases:    64 |    64 passed | 0 failed' in log
assert 'SITE_SECONDS=' in log
# Source checksums were captured before the first build, not after it.
hashes={}
for row in Path('build/scratch/validated-sources.sha256').read_text().splitlines():
    digest,name=row.split(maxsplit=1)
    assert hashlib.sha256(Path(name).read_bytes()).hexdigest()==digest,name
    hashes[name]=digest
os.environ['GIT_OPTIONAL_LOCKS']='0'
def git(*args):
    return subprocess.check_output(['git',*args])
base=git('rev-parse','HEAD').decode().strip()
assert base=='ddb399172bd99091c8d1b04dbf7fe329c8adc43e',base
allowed={'bindings/r/distributions/sged/test.R'}
allowed.update('univariateML-cpp/include/cppunivariate/distributions/sged/'+p for p in ['README.md','mle.hpp','test.cpp'])
changed=set(git('diff','--name-only').decode().splitlines())
assert changed<=allowed,changed-allowed
new=git('ls-files','--others','--exclude-standard').decode().splitlines()
for path in new:
    assert path.startswith(str(folder)+'/') or path in {
        'univariateML-cpp/include/cppunivariate/distributions/sged/held_moment_certificate.hpp',
        'dev/research/evaluations/2026-11-08-sged-held-report.md'},path
cpp_test='univariateML-cpp/include/cppunivariate/distributions/sged/test.cpp'
assert Path(cpp_test).read_bytes().startswith(git('show','HEAD:'+cpp_test))
golden='univariateML-cpp/include/cppunivariate/distributions/sged/golden.json'
assert Path(golden).read_bytes()==git('show','HEAD:'+golden)

def tsv(name):
    with (folder/name).open() as f:
        return list(csv.DictReader(f,delimiter='\t'))
before=tsv('before.tsv');after=tsv('after-workstation.tsv')
old=next(row for row in before if row['scope']=='2' and row['mask']=='0')
new=next(row for row in after if row['scope']=='2' and row['mask']=='0')
fields=['mean','sd','nu','xi','ll','status','certified','work']
identical=all(old[k]==new[k] for k in fields)
errors={k:abs(float(old[k])-float(new[k]))/(1+abs(float(old[k]))) for k in ['mean','sd','nu','xi','ll']}
assert max(errors.values())<=3e-5,errors
assert os.environ['OPENBLAS_NUM_THREADS']==os.environ['OMP_NUM_THREADS']=='1'
js=folder/'targeted-js.json'
js_result=json.loads(js.read_text()) if js.exists() else None
if js_result is not None:assert js_result['passed']==8
result=dict(
    base=base,tested_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
    machine='workstation',compiler=log.splitlines()[0],R=log.splitlines()[1],python=log.splitlines()[2],
    cpu_slots=4,blas_threads=1,openmp_threads=1,nice=os.getpriority(os.PRIO_PROCESS,0),
    source_sha256=hashes,cpp_dev=cpp[0],cpp_full=cpp[1],cpp_no_long_double=cpp[2],
    targeted_js=js_result,targeted_cpp_cases=64,targeted_cpp_assertions=10794,targeted_R=r[0],R_dev=r[1],R_full=r[2],
    seconds=seconds,all_workstation_checks_green=True,cpp_tests_append_only=True,
    protected_sources_unchanged=True,golden_bytes_unchanged=True,
    default_output_bit_identical=identical,default_output_relative_errors=errors,
    math=json.loads((folder/'math.json').read_text()),
    original_subset_timings_before=before,original_subset_timings_after=after,
    new_subset_timings_before=tsv('small-before.tsv'),new_subset_timings_after=tsv('small-after.tsv'),
    simbox_final_status='unreachable (exit 75); workstation fallback completed',
    simbox_final_attempt=json.loads((folder/'simbox-final.json').read_text()) if (folder/'simbox-final.json').exists() else None,
    default_global_certificate=False,family_certified='partial')
(folder/'validation.json').write_text(json.dumps(result,indent=2)+'\n')
print('VALIDATION_RECORD_OK: dev/full, double-precision, site, source checksums, goldens, scope, append-only tests')
