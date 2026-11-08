#!/usr/bin/env python3
"""Archive the final source runs and the maintainer-accepted upstream duplicate."""
import csv,hashlib,json,math,re,subprocess
from pathlib import Path
root=Path.cwd();out=Path(__file__).resolve().parent
sim=(root/'build/scratch/frozen-final-simbox.log').read_text()
local=(root/'build/scratch/frozen-final-local.log').read_text()
files=['univariateML-cpp/include/cppunivariate/distributions/sged/'+p for p in ['mle.hpp','free_shape_certificate.hpp','profile_bounds.hpp','weighted_shape_certificate.hpp','mode_skew_certificate.hpp','test.cpp','README.md','golden.json']]+['bindings/common/distributions/sged/fit.hpp','bindings/r/distributions/sged/test.R','bindings/js/test.mjs']
hashes={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in files}
for path,digest in hashes.items():
    logs=[local] if path=='bindings/js/test.mjs' else [sim] if path=='bindings/r/distributions/sged/test.R' else [sim,local]
    for log in logs:assert f'{digest}  {path}' in log,('changed after final tests',path)
def cpp(log):
    counts=[int(v) for v in re.findall(r'100% tests passed.*?out of (\d+)',log)]
    seconds=[float(v) for v in re.findall(r'Total Test time \(real\) =\s*([\d.]+) sec',log)]
    assert len(counts)==len(seconds)
    return [dict(passed=n,seconds=t) for n,t in zip(counts,seconds)]
cs=cpp(sim);cl=cpp(local)
assert [v['passed'] for v in cs]==[106,129,129],cs
assert [v['passed'] for v in cl]==[129],cl
r=[dict(zip(['fail','warn','skip','pass'],map(int,v))) for v in re.findall(r'^\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]$',sim,re.M)][-2:]
assert len(r)==2 and all(v['fail']==v['warn']==0 for v in r),r
for row,seconds in zip(r,re.findall(r'^Duration: ([\d.]+) s',sim,re.M)[-2:]):row['seconds']=float(seconds)
js={k:int(re.findall(r'^# '+k+r' (\d+)$',local,re.M)[-1]) for k in ['tests','pass','fail']}
assert js['tests']==540 and js['pass']==539 and js['fail']==1,js
known_failure=re.findall(r'^not ok \d+ - (.*)$',local,re.M)
assert known_failure==['binomial relatives retain unresolved finite and Poisson witnesses'],known_failure
new_cases=['SGED symmetric free-shape delegate transports fits and structured stops','SGED balanced endpoint uniform proof covers asymmetric and free skew','SGED weighted free-shape and smooth free-skew profiles certify golden fits','SGED weighted median and smooth reflected half-GED ties retain their witnesses']
assert all(re.search(r'^ok \d+ - '+re.escape(name),local,re.M) for name in new_cases)
assert 'KNOWN upstream stale duplicate only' in local
site=[v for v in local.splitlines() if v.startswith('PASS')]
assert any('99 family pages' in v and 'MathML' in v for v in site),site
assert any('99 generated C++ snippets compile' in v for v in site),site
assert not subprocess.check_output(['git','diff','--check'],text=True)
changed=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
assert all(p.startswith(('univariateML-cpp/include/cppunivariate/distributions/sged/','bindings/common/distributions/sged/','bindings/r/distributions/sged/')) or p=='bindings/js/test.mjs' for p in changed),changed
original=subprocess.check_output(['git','show','HEAD:bindings/js/test.mjs'])
assert (root/'bindings/js/test.mjs').read_bytes().startswith(original)
golden='univariateML-cpp/include/cppunivariate/distributions/sged/golden.json'
assert (root/golden).read_bytes()==subprocess.check_output(['git','show','HEAD:'+golden])
timings={}
for sample in ['small','golden','balanced']:
    before=list(csv.DictReader(out.joinpath('before-'+sample+'.csv').open()))
    after=list(csv.DictReader(out.joinpath('after-'+sample+'.csv').open()))
    assert len(before)==len(after)==32
    for a,b in zip(before,after):
        nu=float(a['nu']);xi=float(a['xi']);mask=int(a['held_mask'])
        changed_route=(math.isnan(nu) and xi==1) or (math.isnan(nu) and mask==0) or (math.isnan(xi) and nu>1 and nu!=2 and mask==0)
        if changed_route:continue
        assert all(a[k]==b[k] for k in a if k!='ms'),(sample,a,b)
    if sample!='balanced':assert all(before[8][k]==after[8][k] for k in before[8] if k!='ms')
    timings[sample]=dict(before=before,after=after)
profile=json.loads(out.joinpath('profile-oracle.json').read_text());assert profile['violations']==0 and profile['interval_checks']==495
unit=json.loads(out.joinpath('math-checks.json').read_text());assert unit['violations']==0
record=dict(all_checks_except_maintainer_accepted_duplicate_green=True,full_js_green=False,known_js_upstream_duplicate=known_failure,maintainer_instruction='Leave the stale duplicate unchanged; it is removed on master and disappears at merge.',new_sged_js_cases_passed=new_cases,timings=timings,default_golden_output_unchanged=True,base=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),source_sha256=hashes,cpp_simbox=cs,cpp_workstation=cl,r_simbox=r,js_workstation=js,site_workstation=site,protected_sources_unchanged=True,js_tests_append_only=True,golden_unchanged=True,math=unit,profiles=profile)
out.joinpath('validation.json').write_text(json.dumps(record,indent=2)+'\n')
summary=[]
for machine,log in [('simbox',sim),('workstation',local)]:
    summary.append(machine.upper())
    summary.extend(line for line in log.splitlines() if re.match(r'^(g\+\+|c\+\+|[0-9a-f]{64}  |100% tests passed|Total Test time|\[ FAIL|Duration:|# (tests|pass|fail|duration_ms)|PASS|KNOWN)',line))
start=local.index('# Subtest: binomial relatives retain unresolved finite and Poisson witnesses');end=local.index('# Subtest:',start+10)
out.joinpath('js-upstream-duplicate.txt').write_text(local[start:end])
out.joinpath('validation.txt').write_text('\n'.join(summary)+'\n')
print(json.dumps(dict(cpp_simbox=cs,cpp_workstation=cl,r_simbox=r,js_workstation=js,site=site),indent=2))
