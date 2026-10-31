#!/usr/bin/env python3
"""Production streaming/checkpoint design from inference-4.py, specialized to
exact pivots and nonsmooth targets. One worker; run via inference-5.sh under
run4 (no nested locks). No installs. Cells are atomically retained and keyed
by source hashes, seed and workload. Independent SciPy endpoint checks precede
Monte Carlo, which tests the actual returned production intervals.
"""
import argparse, hashlib, json, math, platform, subprocess
from pathlib import Path
import numpy as np
from scipy import stats, special
import scipy
ROOT=Path(__file__).resolve().parents[2]
OUT=ROOT/'dev/research/evaluations/inference-5';OUT.mkdir(exist_ok=True)
p=argparse.ArgumentParser();p.add_argument('--reps',type=int,default=5000);p.add_argument('--reference-reps',type=int,default=10000);p.add_argument('--reset',action='store_true');args=p.parse_args()
assert args.reps>=100 and args.reference_reps>=100
SEED=102520261031
families=['laplace','pareto','exp','norm','unif','triang']
inputs=[Path(__file__),ROOT/'dev/validation/inference-5.sh',ROOT/'dev/validation/inference-5-probe.cpp',ROOT/'univariateML-cpp/include/cppunivariate/core/inference.hpp']
for f in families:
    for h in ['inference.hpp','mle.hpp','test.cpp']:
        inputs.append(ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{f}/{h}')
hashes={str(q.relative_to(ROOT)):hashlib.sha256(q.read_bytes()).hexdigest() for q in inputs}
identity=hashlib.sha256(json.dumps(hashes,sort_keys=True).encode()).hexdigest()
probe=subprocess.Popen([str(ROOT/'build/scratch/inference-5-probe')],stdin=subprocess.PIPE,stdout=subprocess.PIPE,text=True,bufsize=1)
def raw(f,x,a=0):
    probe.stdin.write(f'{f} {a:.17g} {len(x)} '+' '.join(format(v,'.17g') for v in x)+'\n');probe.stdin.flush()
    row=np.fromstring(probe.stdout.readline(),sep=' ')
    if not len(row):raise RuntimeError('production bridge ended')
    return row
Z=stats.norm.isf(.025);CRIT=stats.chi2.isf(.05,1)
def close(actual,expected):
    assert np.allclose(actual,expected,rtol=2e-10,atol=2e-12),(actual,expected)
def endpoints():
    x=np.array([.13,.21,.28,.32,.42,.53,.62,.71,.85,.94]);n=len(x);l=x.min();u=x.max();r=u-l
    rows=raw('unif',x).reshape(-1,3);assert not rows[:,0].any()
    qlo=stats.lomax.ppf(.025,n-1);qhi=stats.lomax.isf(.025,n-1)
    close(rows[:,1:],[[l-r*qhi,l-r*qlo],[u+r*qlo,u+r*qhi],r/stats.beta.ppf([.975,.025],n-1,2),u/np.power([.975,.025],1/n),1-(1-l)/np.power([.025,.975],1/n)])
    for f,xx,a,expected in [
      ('exp',x,0,[stats.chi2.ppf([.025,.975],2*n)/(2*x.sum())]),
      ('pareto',1+x,1,[stats.chi2.ppf([.025,.975],2*(n-1))/(2*np.log((1+x)/(1+l)).sum()),stats.chi2.ppf([.025,.975],2*n)/(2*np.log1p(x).sum())]),
      ('norm',x,0,[np.sum((x-x.mean())**2)/stats.chi2.ppf([.975,.025],n-1),np.sum(x*x)/stats.chi2.ppf([.975,.025],n)])]:
        rows=raw(f,xx,a).reshape(-1,3);assert not rows[:,0].any();close(rows[:,1:],expected)
    rows=raw('laplace',x).reshape(-1,3);m=np.sort(x)[n//2];b=np.mean(abs(x-m));h=1.06*np.sort(abs(x-m))[n//2]/math.log(2)*n**(-.2)
    density=np.mean(stats.norm.pdf((x-m)/h))/h
    close(rows[:2,1:],[[m-Z*b/math.sqrt(n),m+Z*b/math.sqrt(n)],[m-Z/(2*density*math.sqrt(n)),m+Z/(2*density*math.sqrt(n))]])
    k=max([0]+[k for k in range(1,n//2+1) if stats.binom.cdf(k-1,n,.5)<=.025]);close(rows[2,1:],[x[k-1],x[n-k]])
    # Production triangular global fit against exhaustive likelihood at all data
    # knots plus design endpoints. Score and LR checked independently.
    row=raw('triang',x,.4)
    def ll(c):
        return n*math.log(2)+np.where(x<c,np.log(x)-math.log(c) if c else -np.inf,np.log1p(-x)-math.log1p(-c) if c<1 else -np.inf).sum()
    vals=np.array([ll(c) for c in [0,*x,1]]);c=[0,*x,1][int(np.argmax(vals))]
    close(row[3],2*(max(vals)-ll(.4)))
    score=np.where(x<.4,-1/.4,1/.6).sum();close(row[1],score*score*.4*.6/n)
    close(row[5:],[c-Z*math.sqrt(c*(1-c)/n),c+Z*math.sqrt(c*(1-c)/n)])
    return 'SciPy endpoint/pivot/KDE comparisons and exhaustive triangular-knot likelihood: passed (rtol 2e-10, atol 2e-12)'
def rng_for(key):
    return np.random.default_rng(SEED+int(hashlib.sha256(key.encode()).hexdigest()[:12],16))
def generate(rng,f,n,regime,parameter):
    if f=='laplace':return rng.laplace(0,1,n) if regime=='model' else rng.normal(0,1,n)
    if f=='pareto':return 2*np.exp(rng.exponential(1/parameter,n))
    if f=='exp':return rng.exponential(1/parameter,n)
    if f=='norm':return rng.normal(2,parameter,n)
    if f=='unif':return rng.uniform(0,1,n)
    return rng.triangular(0,parameter,1,n)
def run():
    check=endpoints();rows=[]
    specs=[('laplace','model',1),('laplace','normal alternative',1),('pareto','model',.7),('pareto','model',3),('exp','model',2),('norm','model',1.5),('unif','model',1),('triang','model',.2),('triang','model',.5),('triang','model',.8)]
    for f,regime,parameter in specs:
      for n in [10,30,100,1000]:
        key=f'{f}-{regime.replace(" ","_")}-{parameter}-{n}';path=OUT/f'{key}.json'
        signature={'identity':identity,'reps':args.reps,'reference_reps':args.reference_reps,'seed':SEED,'key':key}
        if path.exists() and not args.reset:
            saved=json.loads(path.read_text())
            if saved['signature']==signature:rows.extend(saved['rows']);print('reuse',key,flush=True);continue
        rng=rng_for(key);reference=None;critical=None
        if f=='triang':
            rrng=rng_for(key+'-independent-reference');reference=[]
            for _ in range(args.reference_reps):
                q=raw(f,generate(rrng,f,n,regime,parameter),parameter)
                if q[2]!=0:raise ArithmeticError('reference LR failure')
                reference.append(q[3])
            reference=np.sort(reference)
            # Order-statistic critical: acceptance probability k/(M+1) for a
            # continuous null statistic, unconditional over reference runs.
            k=math.ceil(.95*(len(reference)+1));critical=float(reference[k-1])
        names={'laplace':['model Wald','median sandwich','exact sign'],'pareto':['shape endpoint fitted','shape endpoint known'],'exp':['exact rate'],'norm':['variance mean fitted','variance mean known'],'unif':['minimum both fitted','maximum both fitted','width both fitted','maximum minimum known','minimum maximum known'],'triang':['mode Wald','mode score chi2','mode LR chi2','mode LR simulated']}[f]
        hits=np.zeros(len(names),dtype=int);fail=hits.copy()
        truth={'laplace':[0]*3,'pareto':[parameter]*2,'exp':[parameter],'norm':[parameter**2]*2,'unif':[0,1,1,1,0],'triang':[parameter]}[f]
        for _ in range(args.reps):
            x=generate(rng,f,n,regime,parameter);a=2 if f in ['pareto','norm'] else parameter
            q=raw(f,x,a)
            offset=4 if f=='triang' else 0;ci=q[offset:].reshape(-1,3)
            for j,(status,lo,hi) in enumerate(ci):
                if status:fail[j]+=1
                elif lo<=truth[j]<=hi:hits[j]+=1
            if f=='triang':
                for j,status,stat,cutoff in [(1,q[0],q[1],CRIT),(2,q[2],q[3],CRIT),(3,q[2],q[3],critical)]:
                    if status:fail[j]+=1
                    elif stat<=cutoff:hits[j]+=1
        cell=[]
        for j,name in enumerate(names):
            coverage=float(hits[j]/args.reps)
            cell.append({'family':f,'regime':regime,'parameter':parameter,'n':n,'procedure':name,'coverage':coverage,'mcse':math.sqrt(coverage*(1-coverage)/args.reps),'covered':int(hits[j]),'failures':int(fail[j]),'reps':args.reps})
        saved={'signature':signature,'rows':cell,'reference_critical':critical,'reference_mcse_probability':math.sqrt(.95*.05/(args.reference_reps+2)) if reference is not None else None}
        tmp=path.with_suffix('.tmp');tmp.write_text(json.dumps(saved,indent=2)+'\n');tmp.replace(path);rows.extend(cell)
        print(key,[(r['procedure'],round(r['coverage'],4),r['failures']) for r in cell],flush=True)
    metadata={'source_revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),'identity':identity,'hashes':hashes,'seed':SEED,'reps':args.reps,'reference_reps':args.reference_reps,'environment':{'python':platform.python_version(),'numpy':np.__version__,'scipy':scipy.__version__},'independent_checks':check,'rows':rows}
    (OUT/'summary.json').write_text(json.dumps(metadata,indent=2)+'\n')
    assert sum(r['failures'] for r in rows)==0,'procedure failures: inspect retained cells'
    print(check);print('ALL CELLS COMPLETE; zero procedure failures',flush=True)
try:run()
finally:
    probe.stdin.close();probe.stdout.close()
    if probe.wait()!=0:raise RuntimeError('production bridge failed')
