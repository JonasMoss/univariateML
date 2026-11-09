"""Check final paired outputs and record source/data provenance, under one slot."""
from pathlib import Path
import csv,json,hashlib,subprocess,math
p=Path(__file__).resolve().parent
source=Path('univariateML-cpp/include/cppunivariate/distributions/emg')
g=json.loads((source/'golden.json').read_text());summary=[];counts={}
for rep in range(3):
    before=list(csv.DictReader((p/f'before-1e-8-{rep}.csv').open()))
    after=list(csv.DictReader((p/f'after-1e-8-{rep}.csv').open()))
    assert len(before)==len(after)==14
    max_difference=0.;max_ll=0.;max_golden=0.
    for a,b in zip(before,after):
        assert (a['n'],a['mask'],a['status'])==(b['n'],b['mask'],'0'),(a,b)
        mask=int(a['mask']);n=int(a['n'])
        for version,row in [('before',a),('after',b)]:
            key=(version,n,mask)
            value=tuple(row[k] for k in ['certificate_evaluations','profiles','winners'])
            assert counts.setdefault(key,value)==value,(key,counts[key],value)
        for j,name in enumerate(['mu','sigma','lambda']):
            assert a[name]==b[name],(a,b,name)
            av,bv=float(a[name]),float(b[name])
            scale=max(abs(av),float(a['sigma']) if j==0 else 0)
            error=abs(av-bv)/scale
            assert error<=2e-8,(a,b,name,error)
            max_difference=max(max_difference,error)
            if mask&(1<<j):assert bv==g['estimates'][name],(b,name)
            if n==500:
                golden_error=abs(bv-g['estimates'][name])/abs(g['estimates'][name])
                assert golden_error<1e-6,(b,name,golden_error)
                max_golden=max(max_golden,golden_error)
        assert a['logLik']==b['logLik'],(a,b)
        drift=abs(float(a['logLik'])-float(b['logLik']))
        assert drift<1e-9*n,(a,b)
        max_ll=max(max_ll,drift)
    summary.append(dict(repetition=rep,max_normalized_before_after_coefficient_drift=max_difference,max_absolute_loglik_drift=max_ll,max_relative_golden_drift=max_golden))
cpu=[]
last={v:list(csv.DictReader((p/f'{v}-1e-8-2.csv').open())) for v in ['before','after']}
for a,b in zip(last['before'],last['after']):
    ac,bc=float(a['cpu_seconds']),float(b['cpu_seconds'])
    assert ac>0 and bc>0,(a,b)
    cpu.append(dict(n=int(a['n']),mask=int(a['mask']),before_cpu_seconds=ac,after_cpu_seconds=bc,cpu_speedup=ac/bc,before_wall_seconds=float(a['seconds']),after_wall_seconds=float(b['seconds'])))
(p/'cpu-timing.json').write_text(json.dumps(cpu,indent=2)+'\n')
metadata=dict(base_commit='2ed5514b32ec72ded95809c834453d24f025f850',worktree_head=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],flags='-std=c++17 -O2; temporary counters in both binaries',controls=dict(tol=1e-8,maxit=500),n500='frozen EMG golden sample',n10000='seed-162 mt19937_64; Box-Muller normal plus independent exponential at golden coefficients; same sample in both binaries',comparisons=summary,source_sha256={str(q):hashlib.sha256(q.read_bytes()).hexdigest() for q in source.glob('*') if q.is_file()},evidence_sha256={q.name:hashlib.sha256(q.read_bytes()).hexdigest() for q in p.glob('*.py')})
(p/'validation.json').write_text(json.dumps(metadata,indent=2)+'\n')
print(json.dumps(summary,indent=2))
