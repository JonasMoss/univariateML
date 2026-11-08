#!/usr/bin/env python3
"""Independent 70-digit derivatives of the raw profiled log likelihood."""
import itertools,json,subprocess,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
samples=[[-1.,1.],[-1.,1.,1.,1.],[-1.,-.3,0.,.2,.9],[-.8,0.,0.,0.,.9]]
rows=list(itertools.product(samples,[None,.1],[None,.7],[1.,1.001,1.2,1.9,2.,3.,10.],[.1,1e-5]))
root=Path(__file__).resolve().parents[4]
g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/ged/golden.json').read_text())
for mean,sd in itertools.product([None,g['estimates']['mean']],[None,g['estimates']['sd']]):
    rows.append((g['data'],mean,sd,3.01663,.00003))
tie=[0.]*8+[-.32998660809120006,.32998660809120006]*23+[-1.4239025460520294,1.4239025460520294]*12
for mean in [None,0.]:
    rows.extend([(tie,mean,1.,1.10814,.000015),(tie,mean,1.,25.3978,.00006)])
text=''.join(' '.join(map(str,[len(x),p,p+w,'nan' if m is None else m,'nan' if s is None else s,*x]))+'\n' for x,m,s,p,w in rows)
lines=subprocess.run([sys.argv[1]],input=text,text=True,check=True,capture_output=True).stdout.splitlines()
assert len(lines)==len(rows)
cache={}
def point(x,p,m,s):
    x=tuple(map(mp.mpf,x));p=mp.mpf(p);n=len(x)
    key=x,p,m
    if key not in cache:
        if m is not None: loc=mp.mpf(m)
        elif p==1: loc=sorted(x)[n//2]
        else:
            a,b=min(x),max(x)
            for _ in range(150):
                c=(a+b)/2
                g=mp.fsum(mp.sign(c-v)*abs(c-v)**(p-1) for v in x)
                if g>0:b=c
                elif g<0:a=c
                else:a=b=c;break
            loc=(a+b)/2
        cache[key]=loc
    loc=cache[key]
    def raw(mu,nu):
        T=mp.fsum(abs(v-mu)**nu for v in x)/n
        if s is None:
            return -mp.log(2)-mp.loggamma(1+1/nu)-(1+mp.log(nu)+mp.log(T))/nu
        lb=mp.log(s)+(mp.loggamma(1/nu)-mp.loggamma(3/nu))/2
        return -mp.log(2)-lb-mp.loggamma(1+1/nu)-T*mp.exp(-nu*lb)
    # mpmath differentiates the raw density profile independently of C++ formulas.
    score=p*p*mp.diff(lambda v:raw(loc,v),p)
    ls=mp.log(s) if s is not None else (mp.log(p*mp.fsum(abs(v-loc)**p for v in x)/n))/p-(mp.loggamma(1/p)-mp.loggamma(3/p))/2
    derivative=None
    if p>1 and (m is not None or all(v!=loc for v in x)):
        curvature=mp.diff(lambda v:raw(loc,v),p,2)
        if m is None:
            cross=mp.diff(lambda mu:mp.diff(lambda v:raw(mu,v),p),loc)
            mm=mp.diff(lambda mu:raw(mu,p),loc,2)
            curvature-=cross*cross/mm
        derivative=2*score/p+p*p*curvature
    return loc,score,ls,derivative
checks=0
finite_scores=finite_derivatives=0
for row,line in zip(rows,lines):
    x,m,s,p,w=row
    valid,ml,mh,sl,sh,ll,lh,dl,dh=map(lambda z:mp.mpf(float(z)),line.split())
    for v in [p,p+w/2,p+w]:
        loc,score,ls,derivative=point(x,v,m,s)
        assert ml<=loc<=mh,(row,'location',line,str(loc))
        assert sl<=score<=sh,(row,'score',line,str(score))
        assert ll<=ls<=lh,(row,'scale',line,str(ls))
        checks+=3
        finite_scores+=int(mp.isfinite(sl) and mp.isfinite(sh))
        if derivative is not None:
            assert dl<=derivative<=dh,(row,'derivative',line,str(derivative))
            checks+=1
            finite_derivatives+=int(mp.isfinite(dl) and mp.isfinite(dh))
print(json.dumps({'precision':mp.mp.dps,'rows':len(rows),'checks':checks,'finite_score_comparisons':finite_scores,'finite_derivative_comparisons':finite_derivatives,'violations':0},indent=2))
