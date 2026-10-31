#!/usr/bin/env python3
"""Seeded, resumable cell calibration of production fits/information/covariance.
Run under CPU lock. Confidence set coverage is evaluated by test inversion at
truth, avoiding numerical endpoint errors. Independent endpoint checks run first.
Requires existing NumPy/SciPy; never installs packages. One process/thread.
"""
import argparse, hashlib, json, math, os, platform, subprocess
from pathlib import Path
import numpy as np
from scipy import special, optimize, stats
ROOT=Path(__file__).resolve().parents[2]
OUT=ROOT/'dev/research/evaluations/inference-4'; OUT.mkdir(exist_ok=True)
parser=argparse.ArgumentParser();parser.add_argument('--reps',type=int,default=2000)
parser.add_argument('--reset',action='store_true');args=parser.parse_args()
SEED=102420261031; CRIT=stats.chi2.ppf(.95,1); Z=stats.norm.ppf(.975)
inputs=[ROOT/'dev/validation/inference-4.py',ROOT/'dev/validation/inference-4-probe.cpp',ROOT/'univariateML-cpp/include/cppunivariate/core/inference.hpp']
for f in ['norm','gamma','beta','pois']:
    inputs.extend(ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{f}/{h}.hpp' for h in ['score','mle'])
hashes={str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs}
identity=hashlib.sha256(json.dumps(hashes,sort_keys=True).encode()).hexdigest()
probe=subprocess.Popen([str(ROOT/'build/scratch/inference-4-probe')],stdin=subprocess.PIPE,stdout=subprocess.PIPE,text=True,bufsize=1)
def raw(f,x,action,a=1.,b=1.):
    probe.stdin.write(f'{f} {action} {a:.17g} {b:.17g} {len(x)} '+' '.join(format(v,'.17g') for v in x)+'\n');probe.stdin.flush()
    return list(map(float,probe.stdout.readline().split()))
def call(f,x,action='fit',a=1.,b=1.):
    row=raw(f,x,action,a,b)
    if len(row)<8 or row[0] or row[4]: raise ArithmeticError(f'fit status {row[0] if row else -1}, inference {row[4] if len(row)>4 else -1}')
    p=1 if f=='pois' else 2;pos=5;cs=[]
    for _ in range(3):
        status=int(row[pos]);pos+=1;c=np.array(row[pos:pos+p*p]).reshape(p,p);pos+=p*p
        cs.append((status,c))
    return row[1],row[2],row[3],cs,int(row[pos]),row[pos+1]
def root_decreasing(fn):
    # Strictly decreasing derivative in positive nuisance. Expand until signs
    # bracket; exhausting finite floating range FAILS rather than caps estimate.
    lo=hi=1.;fl=fh=fn(1.)
    for _ in range(1000):
        if fl>0: break
        lo/=2;fl=fn(lo)
    for _ in range(1000):
        if fh<0: break
        hi*=2;fh=fn(hi)
    if not(fl>0 and fh<0 and np.isfinite([lo,hi,fl,fh]).all()):raise ArithmeticError('no nuisance bracket')
    r=optimize.brentq(lambda t:fn(math.exp(t)),math.log(lo),math.log(hi),xtol=1e-12)
    v=math.exp(r)
    if not abs(fn(v))<1e-7*(1+abs(fl)+abs(fh)): raise ArithmeticError('nuisance residual')
    return v
def loglik(f,x,a,b):
    n=len(x)
    if f=='norm':return -n*math.log(b)-np.sum((x-a)**2)/(2*b*b)-n*math.log(2*math.pi)/2
    if f=='gamma':return (a-1)*np.log(x).sum()-x.sum()/b-n*(special.gammaln(a)+a*math.log(b))
    if f=='beta':return (a-1)*np.log(x).sum()+(b-1)*np.log1p(-x).sum()-n*special.betaln(a,b)
    return np.sum(special.xlogy(x,a)-a-special.gammaln(x+1))
def null_fit(f,target,x,t):
    n=len(x)
    if f=='norm':a=t;b=math.sqrt(np.mean((x-t)**2))
    elif f=='pois':a=t;b=1.
    elif f=='gamma' and target=='shape':return call(f,x,'shape',t)[:2]
    elif f=='gamma':
        q=np.log(x).mean()-math.log(t)+1-x.mean()/t
        a=root_decreasing(lambda a:math.log(a)-special.digamma(a)+q);b=t/a
    else:
        l=np.log(x).mean();r=np.log1p(-x).mean()
        k=root_decreasing(lambda k:t*(l-special.digamma(t*k))+(1-t)*(r-special.digamma((1-t)*k))+special.digamma(k))
        a=t*k;b=(1-t)*k
    return a,b
def target_gradient(f,target,a,b):
    if f in ['norm','pois']:return a,np.array([1.] if f=='pois' else [1.,0.])
    if target=='shape':return a,np.array([1.,0.])
    if f=='gamma':return a*b,np.array([b,a])
    return a/(a+b),np.array([b,-a])/(a+b)**2

def procedures(f,target,x,truth):
    a,b,_,cs,_,_=call(f,x);est,g=target_gradient(f,target,a,b);out={}
    for name,(status,c) in zip(['wald_F','wald_H','wald_S'],cs):
        v=float(g@c@g)
        if status or not(v>0 and math.isfinite(v)):out[name]=None
        else:out[name]=(est-truth)**2/v<=CRIT
    production=raw(f,x,'targetshape' if target=='shape' else 'targetmean',truth)
    if len(production)!=10:raise ArithmeticError('target protocol')
    if not production[2] and not cs[0][0]:np.testing.assert_allclose(production[3],g@cs[0][1]@g,rtol=2e-7,atol=1e-11)
    out['score']=None if production[6] else production[7]<=CRIT
    out['LR']=None if production[8] else production[9]<=CRIT
    if f=='norm':out['exact']=abs((x.mean()-truth)/(x.std(ddof=1)/math.sqrt(len(x))))<=stats.t.ppf(.975,len(x)-1)
    if f=='pois':
        s=int(x.sum());n=len(x);lo=0 if s==0 else stats.chi2.ppf(.025,2*s)/(2*n)
        hi=stats.chi2.ppf(.975,2*(s+1))/(2*n);out['exact']=lo<=truth<=hi
    return out

# Analytic/independent tests: full covariance cross terms, sandwich, inversion.
def checks():
    for f,x in [('norm',np.array([-2.,-1,0,1,2])),('gamma',np.array([.2,.5,1,2,4.])),('beta',np.array([.1,.2,.4,.6,.8])),('pois',np.array([0.,1,2,3,4]))]:
        a,b,_,cs,_,_=call(f,x);n=len(x)
        if f=='norm':
            z=(x-a)/b;rows=np.array([z/b,(z*z-1)/b]).T;F=np.diag([n/b**2,2*n/b**2]);H=F
        elif f=='gamma':
            rows=np.array([np.log(x/b)-special.digamma(a),(x/b-a)/b]).T
            F=n*np.array([[special.polygamma(1,a),1/b],[1/b,a/b**2]]);H=F
        elif f=='beta':
            rows=np.array([np.log(x)-special.digamma(a)+special.digamma(a+b),np.log1p(-x)-special.digamma(b)+special.digamma(a+b)]).T
            F=n*(np.diag(special.polygamma(1,[a,b]))-special.polygamma(1,a+b));H=F
        else:rows=((x-a)/a)[:,None];F=H=np.array([[n/a]])
        refs=[np.linalg.inv(F),np.linalg.inv(H),np.linalg.solve(H,rows.T)@np.linalg.solve(H,rows.T).T]
        for (s,c),ref in zip(cs,refs):
            assert s==0;np.testing.assert_allclose(c,ref,rtol=1e-8,atol=1e-10)
        target='mean' if f!='pois' else 'rate';truth=target_gradient(f,target,a,b)[0]
        assert all(v for v in procedures(f,target,x,truth).values())
        for t in [truth*.8,truth*1.2] if f!='norm' else [truth-.3,truth+.3]:
            na,nb=null_fit(f,target,x,t)
            reference=call(f,x,'eval',na,nb)
            actual=raw(f,x,'targetmean',t)
            assert actual[6]==actual[8]==0
            np.testing.assert_allclose(actual[7],reference[5],rtol=2e-7,atol=1e-9)
            np.testing.assert_allclose(actual[9],2*(loglik(f,x,a,b)-loglik(f,x,na,nb)),rtol=2e-7,atol=1e-8)
        # Profile LR endpoints are checked crossings. Coverage below uses exact
        # inverted-set membership, not a potentially missed endpoint/root.
        def lr(t):
            na,nb=null_fit(f,target,x,t)
            return 2*(loglik(f,x,a,b)-loglik(f,x,na,nb))-CRIT
        left=truth-1 if f=='norm' else truth/2;right=truth+1 if f=='norm' else min(.999999,truth+(1-truth)/2) if f=='beta' else truth*2
        for _ in range(60):
            if lr(left)>0:break
            left=left*2-truth if f=='norm' else left/2
        for _ in range(60):
            if lr(right)>0:break
            right=right*2-truth if f=='norm' else (1+right)/2 if f=='beta' else right*2
        reference_endpoints=[optimize.brentq(lr,left,truth),optimize.brentq(lr,truth,right)]
        actual=raw(f,x,'interval',0.,CRIT)
        assert actual[0]==0
        np.testing.assert_allclose(actual[1:],reference_endpoints,rtol=2e-7,atol=1e-8)
        for endpoint in actual[1:]:assert abs(lr(endpoint))<1e-6
        if f in ['norm','pois']:
            exact=raw(f,x,'exact');assert exact[0]==0
            if f=='norm':ref=stats.t.interval(.95,n-1,loc=x.mean(),scale=x.std(ddof=1)/math.sqrt(n))
            else:
                total=int(x.sum());ref=[stats.chi2.ppf(.025,2*total)/(2*n),stats.chi2.ppf(.975,2*(total+1))/(2*n)]
            np.testing.assert_allclose(exact[1:],ref,rtol=1e-12,atol=1e-12)
checks()
class BoundaryResult(Exception):
    def __init__(self,result):self.result=result
cells=[]
# Two interior points per target, moderate and challenging; all nuisance fitted.
configs=[('norm','mean',(0.,1.)),('norm','mean',(2.,3.)),('gamma','mean',(.5,2.)),('gamma','mean',(5.,.4)),('gamma','shape',(.5,2.)),('gamma','shape',(5.,.4)),('beta','mean',(.5,.5)),('beta','mean',(2.,5.)),('pois','rate',(.2,1.)),('pois','rate',(5.,1.))]
try:
 for index,(f,target,pars) in enumerate(configs):
  for n in [10,30,100,1000]:
   for miss in [False,True]:
    # One misspecification per family, at first point only (shape target kept).
    if miss and index not in [0,2,4,6,8]:continue
    key=f'{index}-{n}-{int(miss)}';path=OUT/f'cell-{key}.json'
    if path.exists() and not args.reset:
        cell=json.loads(path.read_text())
        if cell['identity']==identity and cell['reps']==args.reps:cells.append(cell);continue
    rng=np.random.default_rng(np.random.SeedSequence([SEED,index,n,int(miss)]));a,b=pars
    truth=target_gradient(f,target,a,b)[0];counts={};failfits=0;boundaries=0;failure_reasons={}
    for rep in range(args.reps):
        if miss:
            if f=='norm':x=a+b*rng.standard_t(5,n)*math.sqrt(3/5)
            elif f=='gamma':
                # Lognormal with same mean and E log X: gamma KL projection
                # retains both shape and mean, not merely moment matching.
                v=2*(math.log(a)-special.digamma(a));x=rng.lognormal(math.log(a*b)-v/2,math.sqrt(v),n)
            elif f=='beta':
                # Symmetric mixture has pseudo-true mean 1/2 by symmetry.
                mix=rng.random(n)<.5;x=np.where(mix,rng.beta(2,8,n),rng.beta(8,2,n))
            else:x=rng.negative_binomial(1,1/(1+a),n)
        elif f=='norm':x=rng.normal(a,b,n)
        elif f=='gamma':x=rng.gamma(a,b,n)
        elif f=='beta':x=rng.beta(a,b,n)
        else:x=rng.poisson(a,n)
        names=['wald_F','wald_H','wald_S'] if miss else ['wald_F','wald_H','wald_S','score','LR']+(['exact'] if f in ['norm','pois'] else [])
        try:
            if f=='pois' and x.sum()==0:
                failfits+=1;boundaries+=1
                failure_reasons['zero-count boundary']=failure_reasons.get('zero-count boundary',0)+1
                # Boundary Wald is unavailable; exact and test inversions still
                # have valid membership calculations. Never drop this replicate.
                result={'score':n*truth<=CRIT,'LR':2*n*truth<=CRIT,
                        'exact':truth<=stats.chi2.ppf(.975,2)/(2*n)}
                raise BoundaryResult(result)
            result=procedures(f,target,x,truth)
        except BoundaryResult as boundary:result=boundary.result
        except (ArithmeticError,ValueError,OverflowError) as exc:
            result={};failfits+=1
            reason=str(exc);failure_reasons[reason]=failure_reasons.get(reason,0)+1
        for name in names:
            c=counts.setdefault(name,dict(covered=0,failed=0,rejected=0));v=result.get(name)
            if v is None:c['failed']+=1
            elif v:c['covered']+=1
            else:c['rejected']+=1
    cell=dict(key=key,family=f,target=target,parameters=pars,n=n,misspecified=miss,reps=args.reps,seed=[SEED,index,n,int(miss)],identity=identity,fit_failures=failfits,boundaries=boundaries,failure_reasons=failure_reasons,procedures=counts)
    temporary=path.with_suffix('.tmp');temporary.write_text(json.dumps(cell,indent=2)+'\n');temporary.replace(path)
    cells.append(cell);print(key,counts,flush=True)
finally:
 probe.stdin.close();probe.wait()
 if probe.returncode:raise RuntimeError('probe failed')
result=dict(seed=SEED,reps=args.reps,source_revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),hashes=hashes,environment=dict(python=platform.python_version(),numpy=np.__version__,scipy=__import__('scipy').__version__),checks='independent covariance checks and profile LR endpoint checks passed',cells=cells)
(OUT/'results.json').write_text(json.dumps(result,indent=2)+'\n')
lines=['| Family/target | Parameters | n | Alternative | Procedure | Coverage ± MCSE | Type I ± MCSE | Failures |','|---|---|---:|---|---|---|---|---:|']
for c in cells:
 for name,v in c['procedures'].items():
    N=c['reps'];p=v['covered']/N;q=v['rejected']/N
    lines.append(f"| {c['family']}/{c['target']} | {c['parameters']} | {c['n']} | {'misspecified' if c['misspecified'] else 'model'} | {name} | {p:.4f} ± {math.sqrt(p*(1-p)/N):.4f} | {q:.4f} ± {math.sqrt(q*(1-q)/N):.4f} | {v['failed']} |")
(OUT/'tables.md').write_text('\n'.join(lines)+'\n')
print('All cells finished; tables and provenance written.',flush=True)
