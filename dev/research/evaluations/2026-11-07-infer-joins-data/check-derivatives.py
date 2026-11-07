#!/usr/bin/env python3
"""80-digit independent scalar derivatives; PIG uses Bessel K, not recurrence."""
import os,sys,json,subprocess,hashlib
from pathlib import Path
import mpmath as mp
ROOT=Path(__file__).resolve().parents[4];OUT=ROOT/'build/scratch/infer-joins';OUT.mkdir(parents=True,exist_ok=True)
INC=ROOT/'univariateML-cpp/include';mp.mp.dps=80
src=r'''#include <iostream>
#include <iomanip>
#include "cppunivariate/core/inference_families.hpp"
int main(){std::string f;unsigned mask;double x;std::cout<<std::setprecision(17);
while(std::cin>>f>>mask>>x){std::array<double,8> p{};for(auto& v:p)std::cin>>v;
auto r=cppunivariate::family_inference({&x,1},f,p,mask);
std::cout<<int(r.status)<<" "<<r.dimension;for(std::size_t i=0;i<r.dimension;++i)std::cout<<" "<<r.score[i];
for(std::size_t i=0;i<r.dimension*r.dimension;++i)std::cout<<" "<<r.observed[i];std::cout<<"\n";}}
'''
(OUT/'probe.cpp').write_text(src)
boost=os.environ.get('BOOST_INCLUDE_DIR',str(Path.home()/'R/x86_64-pc-linux-gnu-library/4.6/BH/include'))
subprocess.run(['ccache','c++','-std=c++17','-O2','-I'+boost,'-I'+str(INC),str(OUT/'probe.cpp'),'-o',str(OUT/'probe')],check=True)

def reference(f,x,p,side):
    if f in ('snorm','sstd'):
        m,sd=p[:2];xi=p[-1]
        if f=='snorm':c0=-mp.log(2*mp.pi)/2;moment=mp.sqrt(2/mp.pi)
        else:
            nu=p[2];c0=mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log((nu-2)*mp.pi)/2
            moment=mp.gamma((nu-1)/2)/mp.gamma(nu/2)*mp.sqrt((nu-2)/mp.pi)
        c=mp.sqrt((1-moment**2)*(xi**2+xi**-2)+2*moment**2-1)
        z=c*(x-m)/sd+moment*(xi-1/xi);q=z*xi if side<0 else z/xi
        base=c0-q*q/2 if f=='snorm' else c0-(nu+1)*mp.log1p(q*q/(nu-2))/2
        return mp.log(c/sd)+mp.log(2/(xi+1/xi))+base
    if f=='huber':
        mu,sd,e=p;z=(x-mu)/sd;Z=mp.sqrt(2*mp.pi)*mp.erf(e/mp.sqrt(2))+2*mp.exp(-e*e/2)/e
        rho=z*z/2 if side==0 else e*(side*z-e/2)
        return -mp.log(sd*Z)-rho
    if f in ('gpd','gev'):
        mu,sd,xi=p;z=(x-mu)/sd
        # At zero use the analytic Taylor limit for the reference derivatives.
        h=z if xi==0 else mp.log1p(xi*z)/xi
        # mp.diff at xi=0 uses tiny nonzero differences with 80 digit context.
        result=-mp.log(sd)-mp.log1p(xi*z)-h
        return result if f=='gpd' else result-mp.exp(-h)
    m,s=p
    # Mixing density IG(mean=m, shape=s) integrated against Poisson.
    a=1+s/(2*m*m);b=s/2;order=x-mp.mpf('.5')
    return mp.log(2)+mp.log(s/(2*mp.pi))/2+s/m-mp.loggamma(x+1)+order*mp.log(b/a)/2+mp.log(mp.besselk(order,2*mp.sqrt(a*b)))

cases=[]
for f in ('snorm','sstd'):
    for xi in (.35,1.,2.4):
        for nu in ([2.15,5.,40.] if f=='sstd' else [0]):
            p=[.2,1.3]+([nu] if f=='sstd' else [])+[xi]
            moment=mp.sqrt(2/mp.pi) if f=='snorm' else mp.gamma((nu-1)/2)/mp.gamma(nu/2)*mp.sqrt((nu-2)/mp.pi)
            c=mp.sqrt((1-moment**2)*(xi**2+xi**-2)+2*moment**2-1)
            join=float(mp.mpf(p[0])-mp.mpf(p[1])*moment*(mp.mpf(xi)-1/mp.mpf(xi))/c)
            for d in (-3.,-1e-5,-1e-9,1e-9,1e-5,3.):cases.append((f,join+d,p,-1 if d<0 else 1))
            # Exact representable symmetric join; asymmetric join rounding is
            # covered from either side, rather than assuming binary equality.
            if xi==1:cases.append((f,p[0],p,1))
for e in (.25,1.,2.5):
    p=[0.,1.,e]
    for d in (-1e-8,0.,1e-8):
        for sign in (-1,1):
            x=sign*(e+d);cases.append(('huber',x,p,0 if d<=0 else sign))
    cases.append(('huber',0.,p,0))
for p in ([2.,3.],[.01,.03],[100.,.2],[2.,10000.],[1000.,10000.]):
    for x in (0,1,2,20,200):cases.append(('pig',x,list(p),0))
for f in ('gpd','gev'):
    for xi in (-.49,-.25,-1e-8,0.,1e-8,.25,.8):
        p=[0.,1.,xi]
        xs=[0.,.2,2.,8.] if xi>=0 else [0.,.2,(1-1e-4)/(-xi),(1-1e-8)/(-xi)]
        if f=='gev':xs+=[-1. if xi<.8 else -.5]
        for x in xs:cases.append((f,x,p,0))
# Every nonempty free subset is checked on representative points, including
# free skew at symmetry. The other cases exercise the full Hessian.
requests=[]
for i,(f,x,p,side) in enumerate(cases):
    masks=(range(2,8,2) if f=='gpd' else range(1,1<<len(p))) if i%11==0 else [6 if f=='gpd' else (1<<len(p))-1]
    for mask in masks:requests.append((f,x,p,side,mask))
text=''.join(f'{f} {mask} {x:.17g} '+ ' '.join(map(str,p+[0.]*(8-len(p))))+'\n' for f,x,p,side,mask in requests)
lines=subprocess.run([str(OUT/'probe')],input=text,text=True,capture_output=True,check=True).stdout.splitlines()
results={f:dict(cases=0,max_score_error=0.,max_observed_error=0.) for f in ('snorm','sstd','huber','pig','gpd','gev')}
assert len(lines)==len(requests)
for case,line in zip(requests,lines):
    f,x,p,side,mask=case;v=list(map(float,line.split()));free=[i for i in range(len(p)) if mask>>i&1];n=len(free)
    assert v[:2]==[0,n],(case,v[:2])
    values=list(map(mp.mpf,p));coord=tuple(values[i] for i in free)
    def fun(*q):
        a=values.copy()
        for i,z in zip(free,q):a[i]=z
        return reference(f,mp.mpf(x),a,side)
    r=results[f];r['cases']+=1
    for i in range(n):
        order=[0]*n;order[i]=1;g=mp.diff(fun,coord,tuple(order));err=float(abs(v[2+i]-g)/(1+abs(g)))
        r['max_score_error']=max(r['max_score_error'],err)
        for j in range(n):
            order=[0]*n;order[i]+=1;order[j]+=1;h=-mp.diff(fun,coord,tuple(order))
            err=float(abs(v[2+n+i*n+j]-h)/(1+abs(h)));r['max_observed_error']=max(r['max_observed_error'],err)
    assert r['max_score_error']<2e-8 and r['max_observed_error']<2e-7,(case,r)
print(json.dumps(results,indent=2))
paths=[Path(__file__),INC/'cppunivariate/core/inference_join.hpp',INC/'cppunivariate/core/inference_numeric_special.hpp',INC/'cppunivariate/core/inference_families.hpp',INC/'cppunivariate/core/inference.hpp']
result=dict(revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),source_sha256={str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in paths},mpmath_version=mp.__version__,mpmath_dps=80,tolerance_score=2e-8,tolerance_observed=2e-7,scaled_by='1+abs(reference)',results=results)
(Path(__file__).parent/'derivatives.json').write_text(json.dumps(result,indent=2)+'\n')
