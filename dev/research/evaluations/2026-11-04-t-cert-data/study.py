"""Research only: fixed-df enclosures and whole-profile value brackets.
No returned proposal is a certified coefficient vector. See derivation.md.
"""
import hashlib
import heapq
import json
import platform
import subprocess
import time
from pathlib import Path
import numpy as np
import scipy
from scipy.optimize import minimize, root, minimize_scalar, brentq
from scipy.special import gammaln

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
EPS=np.finfo(float).eps

def save(name,value):
    (HERE/name).write_text(json.dumps(value,indent=2,allow_nan=False)+'\n')

def clean(v):
    if isinstance(v,dict): return {k:clean(w) for k,w in v.items()}
    if isinstance(v,(list,tuple)): return [clean(w) for w in v]
    if isinstance(v,(np.floating,float)): return float(v) if np.isfinite(v) else None
    if isinstance(v,np.integer): return int(v)
    return v

def normalizer(nu):
    if nu>=1e4:
        # Exact c lies between these two terms; upper endpoint used as centre.
        return -.5*np.log(2*np.pi)-.25/nu+1/(24*nu**3),1/(24*nu**3)
    return gammaln((nu+1)/2)-gammaln(nu/2)-.5*np.log(nu*np.pi),64*EPS*(1+nu*np.log1p(nu))

def evaluate(x,nu,p):
    m,t=p; s=np.exp(t); z=(x-m)/s; den=nu+z*z; r=z*z/den
    c,err=normalizer(nu)
    value=len(x)*(c-t)-.5*(nu+1)*np.log1p(z*z/nu).sum()
    g=np.array([-(nu+1)*np.sum(z/den)/s,len(x)-(nu+1)*r.sum()])
    h=np.array([[(nu+1)*np.sum((nu-z*z)/den**2)/s**2,
                 2*nu*(nu+1)*np.sum(z/den**2)/s],
                [2*nu*(nu+1)*np.sum(z/den**2)/s,
                 2*nu*(nu+1)*np.sum(z*z/den**2)]])
    return value,g,h,err

def scale_bounds(x,nu):
    y=np.sort(x); n=len(y); lower=0.
    for k in range(1,n):
        d=np.min(y[k:]-y[:-k])/2
        lower=max(lower,d*d*((n-k)/n-k/(n*nu)))
    return np.sqrt(lower),np.ptp(x)*np.sqrt(1+1/nu)

def fixed(x,nu):
    L,U=scale_bounds(x,nu)
    if not L>0: raise ArithmeticError('no positive uniform scale containment')
    def f(p):
        v,g,_,_=evaluate(x,nu,p);return -v,g
    p0=[np.median(x),np.log(np.std(x))]
    r=minimize(f,p0,jac=True,method='BFGS',options={'gtol':1e-10,'maxiter':300})
    p=r.x
    # Numerical proposal refinement; enclosure does not rely on solver success.
    rr=root(lambda q:evaluate(x,nu,q)[1],p,jac=lambda q:evaluate(x,nu,q)[2],tol=1e-11)
    if np.all(np.isfinite(rr.x)) and np.linalg.norm(evaluate(x,nu,rr.x)[1])<np.linalg.norm(evaluate(x,nu,p)[1]):p=rr.x
    value,g,_,cerr=evaluate(x,nu,p)
    a=np.sqrt(nu)*np.exp(p[1]); D=0.
    for m in [x.min(),x.max()]:
        for s in [L,U]:
            b=np.sqrt(nu)*s
            D=max(D,np.arccosh(1+((p[0]-m)**2+(a-b)**2)/(2*a*b)))
    # Practical error allowance, not an outward-rounded interval.
    margin=256*EPS*(len(x)+1)*(1+abs(value)+np.sum(np.abs(x)))
    score=np.hypot(a*g[0],g[1])+margin*(1+a)
    return {'nu':nu,'mu':p[0],'scale':np.exp(p[1]),'value':value,
            'lower':value-len(x)*cerr-margin,
            'upper':value+len(x)*cerr+margin+score*D,
            'score_norm':float(np.linalg.norm(g)),'hyperbolic_score':score,
            'diameter':D,'scale_lower':L,'scale_upper':U}

def seam(x):
    z=(x-np.mean(x))/np.std(x);n=len(x)
    m3,m4,m6=[np.mean(z**k) for k in [3,4,6]]
    return {'A':n*(m4-3)/4,'B':n*(m4/4-m6/6+m3*m3/2+(1-m4)**2/4),'m4':m4}

def value_bracket(x,floor,budget=768,tolerance=1e-3):
    n=len(x); normal=-n*(.5*np.log(2*np.pi)+np.log(np.std(x))+.5)
    if budget<35:
        return {'outcome':'budget','normal':normal,'profiles':0,'seam':seam(x)}
    cache={}
    def at(nu):
        if nu not in cache:cache[nu]=fixed(x,nu)
        return cache[nu]
    grid=np.geomspace(floor,1000,33)
    for nu in grid:at(float(nu))
    proposal=max(cache.values(),key=lambda q:q['lower'])
    # Refine best grid mode as a proposal only. All intervals still covered.
    j=np.argmax([at(float(nu))['lower'] for nu in grid])
    if 0<j<len(grid)-1:
        r=minimize_scalar(lambda t:-fixed(x,np.exp(t))['value'],bounds=np.log(grid[[j-1,j+1]]),method='bounded',options={'xatol':1e-11})
        at(float(np.exp(r.x)))
    best=max(cache.values(),key=lambda q:q['lower'])
    gap=best['lower']-normal
    if gap<=tolerance:
        return {'outcome':'open_unsigned_normal_tail','normal':normal,'proposal':best,'profiles':len(cache),'seam':seam(x)}
    # Uniform tail: all nuisance stationary points for nu >= 2 are contained.
    L,U=scale_bounds(x,2.)
    C=1.5*n*(np.ptp(x)/L)**4/4
    N=max(1000.,2*C/gap)
    at(N)
    # Bound c'' by 1/nu^2: chord envelope is max endpoints + n*h^2/(8*lo^2).
    def upper(lo,hi):return max(at(lo)['upper'],at(hi)['upper'])+n*((hi-lo)/lo)**2/8
    knots=sorted(cache);queue=[];counter=0
    for lo,hi in zip(knots[:-1],knots[1:]):
        heapq.heappush(queue,(-upper(lo,hi),counter,lo,hi));counter+=1
    while queue and -queue[0][0]>best['lower']+tolerance and len(cache)<budget:
        _,_,lo,hi=heapq.heappop(queue);mid=np.sqrt(lo*hi);q=at(mid)
        if q['lower']>best['lower']:best=q
        for l,h in [(lo,mid),(mid,hi)]:
            heapq.heappush(queue,(-upper(l,h),counter,l,h));counter+=1
    bound=max(-queue[0][0],normal+C/N)
    return {'outcome':'value_enclosed' if bound<=best['lower']+tolerance else 'budget',
            'lower':best['lower'],'upper':bound,'gap':bound-best['lower'],
            'normal':normal,'proposal':best,'profiles':len(cache),'tail_N':N,
            'tail_upper':normal+C/N,'tolerance':tolerance,'seam':seam(x)}

def inputs():
    rng=np.random.default_rng(14020261104);out=[]
    for family in ['lst','std']:
        g=json.loads((ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
        out.append({'name':family+'-golden','x':g['data']})
    for df in [1,2,3,5,30,1000]:
        for rep in range(2):out.append({'name':f't{df}-{rep}','x':rng.standard_t(df,80).tolist()})
    for rep in range(3):out.append({'name':f'normal-{rep}','x':rng.normal(size=80).tolist()})
    x=rng.normal(size=80);x[:4]=[-20,-9,12,30]
    out.append({'name':'contaminated','x':x.tolist()})
    for e in [.001,.01,.03,.1,.2]:
        x=np.r_[np.arange(-4,0)*e,np.arange(1,5)*e,[-1.01,-.99,.99,1.01]]
        out.append({'name':f'cluster-{e}','x':x.tolist()})
    for target in [-1e-6,0.,1e-6]:
        def cluster(e):return np.r_[np.arange(-4,0)*e,np.arange(1,5)*e,[-1.01,-.99,.99,1.01]]
        e=brentq(lambda e:seam(cluster(e))['A']-target,.001,.01,xtol=1e-15)
        out.append({'name':f'seam-A-{target}','x':cluster(e).tolist()})
    out += [{'name':'ties-half','x':[0,0,-1,1]}, {'name':'ties-two-atoms','x':[0,0,1,1]},
            {'name':'ties-excess','x':[0,0,0,1,2]}, {'name':'ties-two-thirds','x':[0,0,0,0,-1,1]},
            {'name':'ties-admissible','x':[0,0,0,-2,-1,1,2,3]}]
    return out

def main():
    cases=inputs();save('inputs.json',cases);rows=[]
    for case in cases:
        x=np.array(case['x']);n=len(x);k=max(np.unique(x,return_counts=True)[1]);row={'name':case['name'],'n':n,'max_ties':k}
        for family,floor in [('lst',1.),('std',2.)]:
            start=time.perf_counter()
            if k* (1+floor)>=n*floor:
                result={'outcome':'open_collapse_equality' if k*(1+floor)==n*floor else 'proved_unbounded','seam':seam(x)}
            else:result=value_bracket(x,floor)
            result['seconds']=time.perf_counter()-start;row[family]=result
        rows.append(row);print(case['name'],row['lst']['outcome'],row['std']['outcome'],flush=True)
    save('results.json',clean(rows))
    save('environment.json',{'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        'python':platform.python_version(),'numpy':np.__version__,'scipy':scipy.__version__,'platform':platform.platform(),
        'seed':14020261104,'hashes':{p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in [HERE/'study.py',HERE/'inputs.json']}})
if __name__=='__main__':main()
