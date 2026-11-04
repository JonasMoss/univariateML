"""Reproduce the retained outcomes, independent likelihoods and timings.
Run run.sh from the repository root through one CPU slot. No SciPy needed.
"""
import collections
import hashlib
import json
from pathlib import Path
import statistics
import subprocess
import mpmath as mp
HERE=Path(__file__).resolve().parent
BASE=HERE.parent
ROOT=HERE.parents[3]
samples=json.loads((BASE/'2026-11-03-burr-xi-data/samples.json').read_text())
expected={r['sample']:r['outcome'] for r in json.loads((BASE/'2026-11-04-burr-root-data/global.json').read_text())}
roots=json.loads((BASE/'2026-11-04-burr-root-data/verification.json').read_text())['roots']
fixture=(ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/certificate-fixtures.inc').read_text()
# The additional binary64 seam proxy is frozen in both native test files.
line=next(line for line in fixture.splitlines() if line.startswith('{"sign-uncertain-seam"'))
tie=json.loads('['+line.split('{',2)[2].split('}')[0]+']')
samples['sign-uncertain-seam']=tie
expected['sign-uncertain-seam']='tie'
fixture_outcomes={'Ok':'global-finite','WeibullBoundary':'global-Weibull','ParetoBoundary':'global-Pareto','CertificateBudget':'subdivision-budget','CertificateTie':'tie'}
for line in fixture.splitlines():
    if not line.startswith('{"'):continue
    name=line.split('"',2)[1]
    values=json.loads('['+line.split('{',2)[2].split('}')[0]+']')
    assert values==samples[name],name
    declared=line.split('BurrStatus::')[1].split('}')[0]
    assert fixture_outcomes[declared]==expected[name],name
assert len(samples)==51
payload=''.join(str(len(x))+' '+' '.join(format(v,'.17g') for v in x)+'\n' for x in samples.values())
p=subprocess.run([str(ROOT/'build/scratch/burr-port-probe')],input=payload,text=True,capture_output=True,check=True)
assert len(p.stdout.splitlines())==len(samples)
status={0:'global-finite',1:'global-Weibull',2:'global-Pareto',7:'tie',8:'subdivision-budget'}
mp.mp.dps=80
rows=[]
for (name,x),line in zip(samples.items(),p.stdout.splitlines()):
    v=line.split();code=int(v[0]);outcome=status.get(code,'unexpected')
    assert outcome==expected[name],(name,line,expected[name])
    row=dict(sample=name,n=len(x),outcome=outcome,profiles=int(v[5]),seconds=float(v[6]),legacy_seconds=float(v[7]),legacy_status=int(v[8]),seam_slope_enclosure=list(map(float,v[10:12])))
    if code in (0,1,2):
        ll=float(v[4]);row['logLik']=ll
        yy=list(map(lambda t:mp.log(mp.mpf(t)),x))
        if code==0:
            k,c,s=map(lambda t:mp.mpf(float(t)),v[1:4]);z=[c*(y-mp.log(s)) for y in yy]
            exact=len(x)*(mp.log(k)+mp.log(c))-sum(yy)+sum(z)-(k+1)*sum(mp.log1p(mp.exp(t)) for t in z)
            matches=[r for r in roots if r['sample']==name and r['type']=='maximum']
            row['root_relative_error']=float(min(max(abs(a/mp.mpf(r[b])-1) for a,b in zip((k,c,s),('k','c','scale'))) for r in matches))
            assert row['root_relative_error']<1e-8,(name,row)
            row['coefficients']=list(map(float,(k,c,s)))
            # Independent total score, in original (k,c,b) coordinates.
            zz=[c*y-c*mp.log(s) for y in yy];pp=[1/(1+mp.exp(-t)) for t in zz]
            g=[len(x)/k-sum(mp.log1p(mp.exp(t)) for t in zz),len(x)/c+sum(yy)-(k+1)*sum(y*p for y,p in zip(yy,pp)),len(x)-(k+1)*sum(pp)]
            row['score_norm']=float(mp.sqrt(sum(t*t for t in g)))
            assert row['score_norm']<1e-7,(name,row)
        elif code==1:
            c,s=mp.mpf(float(v[2])),mp.mpf(float(v[3]));exact=len(x)*mp.log(c)-sum(yy)+sum(c*(y-mp.log(s))-mp.exp(c*(y-mp.log(s))) for y in yy)
        else:
            b=min(yy);a=len(x)/sum(y-b for y in yy);exact=len(x)*mp.log(a)-sum(yy)-a*sum(y-b for y in yy)
        row['independent_logLik_error']=float(abs(exact-ll))
        assert row['independent_logLik_error']<1e-8,(name,row)
    else:
        if name=='sign-uncertain-seam':assert float(v[10])<=0<=float(v[11])
        assert all(t=='nan' for t in v[1:5]),(name,line)
    rows.append(row)
    print(name,outcome,flush=True)
coordinates=[]
for name,source,factor in [('golden','golden',1),('golden-times-1000','golden',1000),('burr-k0.08-r3','burr-k0.08-r3',1)]:
    root=next(r for r in roots if r['sample']==source and r['type']=='maximum')
    x=[v*factor for v in samples[source]]
    payload=str(len(x))+' '+root['k']+' '+root['c']+' '+format(float(root['scale'])*factor,'.17g')+' '+' '.join(map(str,x))
    out=subprocess.run([str(ROOT/'build/scratch/burr-port-coordinates')],input=payload,text=True,capture_output=True,check=True)
    for line in out.stdout.splitlines():
        mode,ok,delta,fraction=line.split()
        coordinates.append(dict(sample=name,coordinates=['standardized','unshifted logs','logs translated by proposed log scale'][int(mode)],isolated=bool(int(ok)),delta=float(delta),fraction=float(fraction)))
(HERE/'coordinates.json').write_text(json.dumps(coordinates,indent=2)+'\n')
component=subprocess.run([str(ROOT/'build/scratch/burr-port-components')],input=str(len(samples['golden']))+' '+' '.join(map(str,samples['golden'])),text=True,capture_output=True,check=True)
(HERE/'components.json').write_text(json.dumps(json.loads(component.stdout),indent=2)+'\n')
(HERE/'results.json').write_text(json.dumps(rows,indent=2,allow_nan=False)+'\n')
summary=dict(source_revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),
    machine='local workstation; /tmp/umvl-cpu/run; nice 10; one thread',compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
    precision=80,repetitions=10,discarded_warmups=1,outcomes=dict(collections.Counter(r['outcome'] for r in rows)),
    median_seconds=statistics.median(r['seconds'] for r in rows[:50]),legacy_median_seconds=statistics.median(r['legacy_seconds'] for r in rows[:50]),
    golden_seconds=rows[0]['seconds'],golden_legacy_seconds=rows[0]['legacy_seconds'],
    max_root_relative_error=max(r.get('root_relative_error',0) for r in rows),max_independent_logLik_error=max(r.get('independent_logLik_error',0) for r in rows),
    sha256={str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [HERE/'probe.cpp',HERE/'check.py',ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/certificate.hpp',ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/mle.hpp',ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/certificate-fixtures.inc',
      HERE/'components.cpp',HERE/'coordinates.cpp',HERE/'run.sh',HERE/'validation.py',
      BASE/'2026-11-03-burr-xi-data/samples.json',BASE/'2026-11-04-burr-root-data/global.json',BASE/'2026-11-04-burr-root-data/verification.json',
      ROOT/'bindings/common/distributions/burr/fit.hpp',ROOT/'bindings/r/distributions/burr/test.R',
      ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/README.md',ROOT/'univariateML-cpp/include/cppunivariate/distributions/burr/test.cpp',
      ROOT/'univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp',ROOT/'univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp',ROOT/'univariateML-cpp/include/cppunivariate/distributions/pareto4/mle.hpp']})
(HERE/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
