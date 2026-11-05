#!/usr/bin/env python3
"""Freeze final local suite results and verify the tested source fingerprints.
Run through /tmp/umvl-cpu/run after the foreground suite commands finish.
Logs are expected under build/scratch, as recorded in the job report.
"""
import ast
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
LOG=ROOT/'build/scratch'
REPORT=ROOT/'dev/research/evaluations/2026-11-05-gpd-cert-report.md'
os.environ['GIT_OPTIONAL_LOCKS']='0'

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

numerical=json.loads((HERE/'certificate-validation.json').read_text())
for path,value in numerical['hashes'].items():
    assert digest(ROOT/path)==value,f'fingerprint changed: {path}'
assert len(numerical['records'])==22
assert all(r['status']==0 for r in numerical['records'])
unique=json.loads((HERE/'uniqueness-validation.json').read_text())
assert [x['status'] for x in unique['cases']]==[0,3,0]
assert unique['cases'][1]['certificate_failure']==3
base=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
assert base==numerical['base']
golden='univariateML-cpp/include/cppunivariate/distributions/gpd/golden.json'
assert subprocess.check_output(['git','show',f'{base}:{golden}'],cwd=ROOT)==(ROOT/golden).read_bytes()
subprocess.run(['git','diff','--check'],cwd=ROOT,check=True)
changed=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
allowed={
 'bindings/common/distributions/gpd/fit.hpp','bindings/r/distributions/gpd/test.R',
 'univariateML-cpp/include/cppunivariate/distributions/gpd/README.md',
 'univariateML-cpp/include/cppunivariate/distributions/gpd/mle.hpp',
 'univariateML-cpp/include/cppunivariate/distributions/gpd/test.cpp'}
assert set(changed)==allowed,changed
untracked=subprocess.check_output(['git','ls-files','--others','--exclude-standard'],cwd=ROOT,text=True).splitlines()
for path in untracked:
    assert path.startswith(str(HERE.relative_to(ROOT))+'/') or path in {
        str(REPORT.relative_to(ROOT)),
        'univariateML-cpp/include/cppunivariate/distributions/gpd/certificate.hpp'},path
for path in HERE.glob('*.py'):
    ast.parse(path.read_text(),filename=str(path))
for path in [ROOT/'univariateML-cpp/include/cppunivariate/distributions/gpd/certificate.hpp',*HERE.glob('*.py'),REPORT]:
    if not path.exists():continue
    value=path.read_text()
    assert value.endswith('\n'),path
    assert all(line.rstrip(' \t')==line for line in value.splitlines()),path
for path in [ROOT/'univariateML-cpp/include/cppunivariate/distributions/gpd/README.md',REPORT]:
    if not path.exists():continue
    for target in re.findall(r'\[[^\]]+\]\(([^)]+)\)',path.read_text()):
        if target.startswith(('https:','http:','mailto:','#')):continue
        target=target.strip('<>').split('#',1)[0]
        assert (path.parent/target).resolve().exists(),(path,target)

assert 'All final suites passed' in (LOG/'final-suite-progress.log').read_text()
suites=[]
for label,filename,command,expected in [
 ('C++ dev','final-cpp-dev.log','just cpp-test',106),
 ('C++ no-long-double full','final-cpp-no-long-double.log','just cpp-test-no-long-double',127),
 ('C++ full','final-cpp-full.log','UMVL_TEST_TIER=full just cpp-test',127)]:
    output=(LOG/filename).read_text()
    count=re.findall(r'100% tests passed, 0 tests failed out of (\d+)',output)
    assert count and int(count[-1])==expected,(label,count)
    elapsed=re.findall(r'Total Test time \(real\) =\s*([\d.]+) sec',output)
    suites.append(dict(name=label,command=command,machine='local workstation',slots=4,
                       exit_code=0,passed=expected,failed=0,elapsed_seconds=float(elapsed[-1])))
for label,filename,command,skip in [
 ('R dev','final-r-dev.log','just test',154),
 ('R full','final-r-full.log','just test-full',0),
 ('R GPD target serial','gpd-r1-target.log','UMVL_TEST_TIER=full testthat::test_local(filter="dist-gpd")',0)]:
    output=(LOG/filename).read_text()
    values=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',output)
    assert values,(label,'missing summary')
    fail,warn,skipped,passed=map(int,values[-1])
    assert (fail,warn,skipped)==(0,0,skip),(label,values[-1])
    suites.append(dict(name=label,command=command,machine='local workstation',
                       slots=1 if label.endswith('serial') else 4,
                       exit_code=0,passed=passed,failed=fail,warnings=warn,skipped=skipped))
result=dict(base=base,permitted_failures=0,suites=suites,
            numerical_workloads=len(numerical['records']),
            maximum_scale_relative_error=max(x['scale_relative_error'] for x in numerical['records']),
            maximum_shape_scaled_error=max(x['shape_scaled_error'] for x in numerical['records']),
            maximum_mean_loglik_error=max(x['mean_loglik_error'] for x in numerical['records']),
            source_hashes=numerical['hashes'],
            evidence_hashes={p.name:digest(p) for p in [HERE/'certificate-validation.json',HERE/'uniqueness-validation.json',HERE/'environment.txt',Path(__file__).resolve()]})
(HERE/'test-summary.json').write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
print(json.dumps(result,indent=2))
