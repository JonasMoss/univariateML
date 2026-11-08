#!/usr/bin/env python3
"""Archive final completed O1 suites; run only after both commands exit zero."""
import csv,hashlib,json,math,re,statistics,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[4];out=Path(__file__).resolve().parent
sim=(root/'build/scratch/o1-final-simbox.log').read_text()
local=(root/'build/scratch/o1-final-local.log').read_text()
oracles=(root/'build/scratch/o1-oracles-final.log').read_text()
timing=(root/'build/scratch/o1-timings-final.log').read_text()
case=(root/'build/scratch/o1-cases.log').read_text()
base='d34a44cd1b0d7324e1f486d66e6b3f7e3db9210a'
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==base
files=['univariateML-cpp/include/cppunivariate/distributions/ged/'+v for v in ['mle.hpp','free_shape_bounds.hpp','shape_certificate.hpp','test.cpp','README.md']]+['bindings/common/distributions/ged/fit.hpp','bindings/r/distributions/ged/test.R','bindings/js/test.mjs']
hashes={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in files}
for path,digest in hashes.items():
    # JS-only regression updates do not affect native C++/R runs.
    log=local if path=='bindings/js/test.mjs' or path.endswith('/README.md') else sim
    assert f'{digest}  {path}' in log,('source changed since final tests',path)
def cpp(text):
    counts=list(map(int,re.findall(r'100% tests passed.*?out of (\d+)',text)))
    seconds=list(map(float,re.findall(r'Total Test time \(real\) =\s*([\d.]+) sec',text)))
    assert len(counts)==len(seconds)
    return [dict(passed=n,seconds=s) for n,s in zip(counts,seconds)]
cs,cl=cpp(sim),cpp(local)
assert [v['passed'] for v in cs]==[106,129,129]
assert [v['passed'] for v in cl]==[129]
r=[dict(zip(['fail','warn','skip','pass'],map(int,row))) for row in re.findall(r'^\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]$',sim,re.M)]
r=r[-2:]
assert len(r)==2 and all(v['fail']==v['warn']==0 for v in r),r
for row,seconds in zip(r,re.findall(r'^Duration: ([\d.]+) s',sim,re.M)[-2:]):row['seconds']=float(seconds)
js={k:int(re.findall(r'^# '+k+r' (\d+)$',local,re.M)[-1]) for k in ['tests','pass','fail']}
assert js['tests']==js['pass'] and js['fail']==0
js['seconds']=float(re.findall(r'^# duration_ms ([\d.]+)$',local,re.M)[-1])/1000
site=[v for v in local.splitlines() if v.startswith('PASS')]
assert any('99 family pages' in v and 'MathML' in v for v in site)
assert any('99 generated C++ snippets compile' in v for v in site)
objects=[]
for match in re.finditer(r'^\{',oracles,re.M):
    try:objects.append(json.JSONDecoder().raw_decode(oracles[match.start():])[0])
    except json.JSONDecodeError:pass
score=next(v for v in objects if 'checks' in v);roots=next(v for v in objects if isinstance(v.get('rows'),list))
assert score['violations']==roots['violations']==0
before,after=timing.split('\nBEFORE\n')[1].split('\nAFTER\n')
def rows(text):return [line.split() for line in text.splitlines() if re.match(r'^[01] (500|10000) [0-3] ',line)]
a,b=rows(before),rows(after)
assert len(a)==len(b)==32 and all(int(v[4])==0 and int(v[5])==1 for v in b)
labels={0:'mean,sd,nu',1:'sd,nu',2:'mean,nu',3:'nu'}
values=[]
for sample in [0,1]:
    for mask in range(4):
        ba=[v for v in a if int(v[0])==sample and int(v[2])==mask]
        af=[v for v in b if int(v[0])==sample and int(v[2])==mask]
        assert len(ba)==len(af)==(3 if sample else 5)
        values.append(dict(sample='quantiles-10000' if sample else 'frozen-golden',n=int(af[0][1]),free=labels[mask],repetitions=len(af),before_ms=statistics.median(float(v[11]) for v in ba),after_ms=statistics.median(float(v[11]) for v in af),certificate_evaluations=int(af[0][10]),mean=float(af[0][6]),sd=float(af[0][7]),nu=float(af[0][8]),logLik=float(af[0][9])))
def num(v):
    x=float(v);return x if math.isfinite(x) else ('+Inf' if x>0 else '-Inf')
cases=[]
for line in case.splitlines():
    mask,it,status,cert,budget,witnesses,lo,hi=line.split()
    cases.append(dict(mask=int(mask),iterlim=int(it),status=int(status),certified=bool(int(cert)),budget=bool(int(budget)),witnesses=int(witnesses),lower=num(lo),upper=num(hi)))
assert len(cases)==16
assert any(v['budget'] and v['witnesses'] for v in cases)
assert all(v['certified'] for v in cases if v['iterlim']==200)
for path in ['bindings/js/test.mjs']:
    assert (root/path).read_bytes().startswith(subprocess.check_output(['git','show',base+':'+path],cwd=root))
protected=['univariateML-cpp/include/cppunivariate/distributions/ged/'+v for v in ['dpqr.hpp','truth.py','truth.json']]
protected+=subprocess.check_output(['git','ls-tree','-r','--name-only',base,'univariateML-cpp/include/cppunivariate/distributions/sged'],cwd=root,text=True).splitlines()
for path in protected:
    assert (root/path).read_bytes()==subprocess.check_output(['git','show',base+':'+path],cwd=root),path
golden='univariateML-cpp/include/cppunivariate/distributions/ged/golden.json'
assert (root/golden).read_bytes()==subprocess.check_output(['git','show',base+':'+golden],cwd=root)
normalization_before=(root/'build/scratch/o1-normalization-before.log').read_text().strip()
normalization_after=(root/'build/scratch/o1-normalization-after.log').read_text().strip()
assert 'status 0 certified 1' in normalization_before
assert 'status 7 certified 0' in normalization_after
normalization_discrepancy=float(normalization_after.rsplit('discrepancy ',1)[1])
assert abs(normalization_discrepancy)<1e-8
postconditions=(root/'build/scratch/o1-postconditions.log').read_text().strip()
fixed_before=(root/'build/scratch/o1-fixed-scale-before.log').read_text().strip()
fixed_after=(root/'build/scratch/o1-fixed-scale-after.log').read_text().strip()
assert 'status 0 certified 1' in fixed_before and 'status 7 certified 0' in fixed_after
assert 'support 7' in postconditions and 'median 5' in postconditions
record=dict(protected_files_unchanged=protected,shared_js_tests_append_only=True,postconditions=postconditions,fixed_scale_precision=dict(before=fixed_before,after=fixed_after),protected_density_open=json.loads((out/'density-underflow.json').read_text()),normalization=dict(before=normalization_before,after=normalization_after),base_commit=base,source_sha256=hashes,family_certified='yes',open_O1='closed',golden_bytes_unchanged=True,default_iterlim=200,arithmetic='Exact arithmetic conditional on computed comparisons, with practical floating-point margins; no outward-rounded interval claim.',simbox=dict(cpp_compiler='GCC 13.3.0',r_cpp_compiler='g++-15',cpp_dev=cs[0],cpp_full=cs[1],cpp_no_long_double=cs[2],r_dev=r[0],r_full=r[1]),workstation=dict(cpp_compiler='GCC 15.2.0',cpp_no_long_double=cl[0],js=js,site_checks=site),score_enclosure_oracle=score,golden_root_oracle=roots,benchmark=dict(machine='simbox',flags='-std=c++17 -O2',gradtol=1.220703e-4,iterlim=200,summary=values,raw_before=a,raw_after=b),control_probes=cases,separated_root_tie=json.loads((out/'shape-tie.json').read_text()))
(out/'validation.json').write_text(json.dumps(record,indent=2,allow_nan=False)+'\n')
for filename,records in [('timings.tsv',values),('o1-control-probes.tsv',cases)]:
    with (out/filename).open('w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(records[0]),delimiter='\t');w.writeheader();w.writerows(records)
print('Final source hashes, suites, independent roots, timings, controls and frozen golden verified.')
