#!/usr/bin/env python3
"""Independent mpmath checks of explicitly conditional inference submodels.

Golden fitted structural/nonsmooth coefficients are identified as conditional
checks, never treated as genuinely known for inference. For unavailable golden
regimes, test their returned status and evaluate a separate seeded submodel.
"""
import hashlib
import json
import math
import os
from pathlib import Path
import random
import shutil
import subprocess
import sys
import mpmath as mp

ROOT=Path(__file__).resolve().parents[2]
os.environ.setdefault('CCACHE_DIR',str(Path.home()/'.cache/ccache/univariateML'))
os.environ.setdefault('CCACHE_BASEDIR',str(ROOT));os.environ.setdefault('CCACHE_NOHASHDIR','1')
COMPILER=([shutil.which('ccache')] if shutil.which('ccache') else [])+[os.environ.get('CXX','c++')]
INC=ROOT/'univariateML-cpp/include/cppunivariate';DIST=INC/'distributions'
OUT=ROOT/'build/scratch/inference-numeric-special';OUT.mkdir(parents=True,exist_ok=True)
FAMILIES={'laplace':(['mu','sigma'],[1]),'gev':(['mu','sigma','xi'],[0,1]),
'gpd':(['mu','sigma','xi'],[1,2]),'ged':(['mean','sd','nu'],[0,1,2]),
'sged':(['mean','sd','nu','xi'],[0,1,2,3]),'snorm':(['mean','sd','xi'],[0,1]),
'sstd':(['mean','sd','nu','xi'],[0,1,2]),'tnorm':(['mean','sd','a','b'],[0,1]),
'cmb':(['size','prob','nu'],[1,2]),'cmp':(['lambda','nu'],[0,1]),
'tbinom':(['prob','a','b','size'],[0]),'tpois':(['lambda','a','b'],[0]),
'zmbinom':(['size','prob','p0'],[1,2])}
seed_only={'gev','snorm','sstd'}
SEED=20261102;rng=random.Random(SEED);mp.mp.dps=40
src='''#include <array>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include "cppunivariate/core/inference_families.hpp"
int main(){std::cout<<std::setprecision(17);std::string f,t;std::size_t count;unsigned mask;
while(std::cin>>f>>count>>mask){std::array<double,cppunivariate::inference_max_parameters> p{};
for(auto& v:p){std::cin>>t;v=std::stod(t);}std::vector<double> d(count);for(auto& x:d)std::cin>>x;
const auto r=cppunivariate::family_inference({d.data(),d.size()},f,p,mask);
std::cout<<static_cast<int>(r.status)<<" "<<r.dimension;
for(std::size_t i=0;i<r.dimension;++i)std::cout<<" "<<r.score[i];
for(std::size_t i=0;i<r.dimension*r.dimension;++i)std::cout<<" "<<r.observed[i];std::cout<<"\\n";}}
'''
(OUT/'probe.cpp').write_text(src)
boost=os.environ.get('BOOST_INCLUDE_DIR','');cache=ROOT/'univariateML-cpp/build/CMakeCache.txt'
if not boost and cache.exists(): boost=next((l.split('=',1)[1] for l in cache.read_text().splitlines() if l.startswith('Boost_INCLUDE_DIR:')),'')
subprocess.run(COMPILER+['-std=c++17','-O2','-I'+boost,'-I'+str(INC.parent),str(OUT/'probe.cpp'),'-o',str(OUT/'probe')],check=True)

def logdensity(f,x,p):
    if f=='laplace':
        mu,sd=p;return -mp.log(2*sd)-abs(x-mu)/sd
    if f=='gev':
        mu,sd,_=p;z=(x-mu)/sd;return -mp.log(sd)-z-mp.exp(-z)
    if f=='gpd':
        mu,sd,xi=p;z=(x-mu)/sd
        return -mp.log(sd)-z if xi==0 else -mp.log(sd)-(1+xi)*mp.log1p(xi*z)/xi
    if f in ('ged','sged'):
        mu,sd,nu=p[:3];b=mp.sqrt(mp.power(2,-2/nu)*mp.gamma(1/nu)/mp.gamma(3/nu))
        if f=='ged':
            return -mp.log(sd)-abs(x-mu)**nu/(2*(sd*b)**nu)+mp.log(nu)-3*mp.loggamma(1/nu)/2+mp.loggamma(3/nu)/2-mp.log(2)
        xi=p[3];m=mp.gamma(2/nu)/mp.sqrt(mp.gamma(1/nu)*mp.gamma(3/nu))
        c=mp.sqrt((1-m*m)*(xi*xi+1/(xi*xi))+2*m*m-1)
        z=c*(x-mu)/sd+m*(xi-1/xi);q=z/xi if z>0 else z*xi
        return mp.log(c)-mp.log(sd)+mp.log(2/(xi+1/xi))+mp.log(nu/(b*mp.power(2,1+1/nu)*mp.gamma(1/nu)))-abs(q)**nu/(2*b**nu)
    if f=='snorm':
        mu,sd,_=p;z=(x-mu)/sd;return -mp.log(sd)-mp.log(2*mp.pi)/2-z*z/2
    if f=='sstd':
        mu,sd,nu,_=p;z=(x-mu)/sd
        return mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log((nu-2)*mp.pi)/2-mp.log(sd)-(nu+1)*mp.log1p(z*z/(nu-2))/2
    if f=='tnorm':
        mu,sd,a,b=p;cdf=lambda z:mp.erfc(-z/mp.sqrt(2))/2
        return -mp.log(sd)-mp.log(2*mp.pi)/2-((x-mu)/sd)**2/2-mp.log(cdf((b-mu)/sd)-cdf((a-mu)/sd))
    if f=='cmb':
        size,prob,nu=p;N=int(size)
        term=lambda k:mp.power(mp.binomial(N,k),nu)*prob**k*(1-prob)**(N-k)
        return nu*mp.log(mp.binomial(N,int(x)))+x*mp.log(prob)+(N-x)*mp.log1p(-prob)-mp.log(mp.fsum(term(k) for k in range(N+1)))
    if f=='cmp':
        lam,nu=p
        z=mp.nsum(lambda k:mp.exp(k*mp.log(lam)-nu*mp.loggamma(k+1)),[0,mp.inf])
        return x*mp.log(lam)-nu*mp.loggamma(x+1)-mp.log(z)
    if f=='tbinom':
        prob,a,b,size=p;N=int(size);lo=max(0,int(mp.floor(a))+1);hi=min(N,int(mp.floor(b)))
        term=lambda k:mp.binomial(N,k)*prob**k*(1-prob)**(N-k)
        return mp.log(term(int(x)))-mp.log(mp.fsum(term(k) for k in range(lo,hi+1)))
    if f=='tpois':
        lam,a,b=p;lo=max(0,int(mp.floor(a))+1);hi=int(mp.floor(b))
        return x*mp.log(lam)-mp.loggamma(x+1)-mp.log(mp.fsum(lam**k/mp.factorial(k) for k in range(lo,hi+1)))
    if f=='zmbinom':
        size,prob,p0=p;N=int(size)
        if x==0:return mp.log(p0)
        return mp.log1p(-p0)+mp.log(mp.binomial(N,int(x)))+x*mp.log(prob)+(N-x)*mp.log1p(-prob)-mp.log1p(-(1-prob)**N)
    raise ValueError(f)

def direct_derivatives(f,x,p,free):
    fixed=list(map(mp.mpf,p));coordinates=tuple(fixed[i] for i in free)
    def fn(*q):
        values=fixed.copy()
        for i,v in zip(free,q): values[i]=v
        return logdensity(f,mp.mpf(x),values)
    grad=[];hess=[]
    for i in range(len(free)):
        order=[0]*len(free);order[i]=1;grad.append(mp.diff(fn,coordinates,tuple(order)))
        row=[]
        for j in range(len(free)):
            order=[0]*len(free);order[i]+=1;order[j]+=1;row.append(-mp.diff(fn,coordinates,tuple(order)))
        hess.append(row)
    return grad,hess

def run(f,x,p,mask):
    q=list(p)+[0.]*(8-len(p))
    text=f'{f} 1 {mask} '+' '.join(str(v) for v in q)+f' {x:.17g}\n'
    line=subprocess.run([str(OUT/'probe')],input=text,text=True,capture_output=True,check=True).stdout.strip()
    return list(map(float,line.split()))

results={}
for f,(names,free) in FAMILIES.items():
    item={'status':'open','reason':'not evaluated'};results[f]=item
    try:
        g=json.loads((DIST/f/'golden.json').read_text());p=[float(g['estimates'][k]) for k in names]
        mask=sum(1<<i for i in free);n=len(free);cases=[]
        if f in seed_only:
            true_mask=(1<<len(names))-1
            status=int(run(f,g['data'][0],p,true_mask)[0])
            if status!=11: raise ValueError(f'expected RegimeUnavailable at default golden, got {status}')
            item['default_golden_status']='RegimeUnavailable'
            p[-1]=0 if f=='gev' else 1
        else:
            cases.extend((x,p,'golden') for x in g['data'])
            if f=='laplace':item['conditioned_on_fitted']='mu'
            if f=='zmbinom' and (g.get('fit_args') or {}).get('size') is None:item['conditioned_on_fitted']='size'
        for _ in range(5):
            q=p.copy()
            for i in free:
                name=names[i]
                if name in ('prob','p0'):q[i]=1/(1+math.exp(-(math.log(q[i]/(1-q[i]))+.05)))
                elif name=='nu' and f=='sstd':q[i]=2+1.03*(q[i]-2)
                elif name in ('sigma','sd','nu','lambda'):q[i]*=1.03
                elif name=='xi' and f=='gpd':q[i]*=1.03
                else:q[i]+=.03
            cases.append((rng.choice(g['data']),q,'seeded'))
        def request(xs,q):
            return f'{f} {len(xs)} {mask} '+' '.join(str(v) for v in (list(q)+[0.]*(8-len(q))))+' '+' '.join(f'{x:.17g}' for x in xs)+'\n'
        data=''.join(request([x],q) for x,q,_ in cases)
        if f not in seed_only:data+=request(g['data'],p)
        lines=subprocess.run([str(OUT/'probe')],input=data,text=True,capture_output=True,check=True).stdout.splitlines()
        if len(lines)!=len(cases)+(f not in seed_only):raise ValueError('probe output count mismatch')
        aggregate=list(map(float,lines.pop().split())) if f not in seed_only else None
        if aggregate is not None and (int(aggregate[0])!=0 or int(aggregate[1])!=n):raise ValueError(f'full-data production status {aggregate[0]}')
        maxscore=maxh=0.;total=[0.]*n;refscore=[mp.mpf('0')]*n;totalh=[0.]*(n*n);refh=[mp.mpf('0')]*(n*n)
        # CMP normalizer derivatives do not depend on x; cache by parameter vector.
        cmp_cache={}
        for (x,q,kind),line in zip(cases,lines):
            v=list(map(float,line.split()));status,dimension=int(v[0]),int(v[1])
            if status!=0 or dimension!=n: raise ValueError(f'production status {status}, dimension {dimension}, case {kind}')
            if f=='cmp':
                key=tuple(q)
                if key not in cmp_cache:
                    g0,h0=direct_derivatives(f,0,q,free);cmp_cache[key]=(g0,h0)
                g0,h0=cmp_cache[key];refg=[g0[0]+mp.mpf(x)/q[0],g0[1]-mp.loggamma(x+1)]
                refhess=[[h0[0][0]+mp.mpf(x)/(mp.mpf(q[0])**2),h0[0][1]],[h0[1][0],h0[1][1]]]
            else:refg,refhess=direct_derivatives(f,x,q,free)
            for i in range(n):
                err=float(abs(v[2+i]-refg[i])/(1+abs(refg[i])));maxscore=max(maxscore,err)
                if not math.isfinite(err):raise ValueError('nonfinite score error')
                if kind=='golden':total[i]+=v[2+i];refscore[i]+=refg[i]
                for j in range(n):
                    k=i*n+j;err=float(abs(v[2+n+k]-refhess[i][j])/(1+abs(refhess[i][j])));maxh=max(maxh,err)
                    if not math.isfinite(err):raise ValueError('nonfinite Hessian error')
                    if kind=='golden':totalh[k]+=v[2+n+k];refh[k]+=refhess[i][j]
        if aggregate is not None:
            total=aggregate[2:2+n];totalh=aggregate[2+n:2+n+n*n]
        se=max(float(abs(a-b)/(1+abs(b))) for a,b in zip(total,refscore)) if f not in seed_only else None
        he=max(float(abs(a-b)/(1+abs(b))) for a,b in zip(totalh,refh)) if f not in seed_only else None
        item.update(status='pass',reason='',golden_observations=0 if f in seed_only else len(g['data']),seeded=5,
                    free=[names[i] for i in free],max_score_error=maxscore,max_observed_error=maxh,
                    total_score_error=se,total_observed_error=he,golden_total_score=total if f not in seed_only else None)
        if maxscore>=2e-6 or maxh>=2e-5 or (se is not None and (se>=2e-6 or he>=2e-5 or max(map(abs,total))>=1e-3)):
            item.update(status='fail',reason='scaled-error or golden-score tolerance exceeded')
    except Exception as exc:item.update(status='open',reason=f'{type(exc).__name__}: {exc}')
    print(f'{f}: {item["status"]} {item["reason"]}',flush=True)
# Selected conditional charts and special members, including exact joins.
try:
    cases=[('cmb',[10.,.4,0.],[1,2],[0.,3.,10.]),
           ('gpd',[0.,1.,0.],[1],[0.,.5,2.]),
           ('tnorm',[0.,1.,-1.,2.],[0,1],[-.8,0.,1.7]),
           ('ged',[0.,1.,1.5],[1,2],[-1.,0.,1.])]
    extra={}
    for family,p,free,xs in cases:
        mask=sum(1<<i for i in free);n=len(free);ms=mh=0.
        for x in xs:
            v=run(family,x,p,mask)
            if int(v[0])!=0 or int(v[1])!=n:raise ValueError(f'{family}: status {v[0]}')
            g,h=direct_derivatives(family,x,p,free)
            for i in range(n):
                ms=max(ms,float(abs(v[2+i]-g[i])/(1+abs(g[i]))))
                for j in range(n):
                    k=i*n+j;mh=max(mh,float(abs(v[2+n+k]-h[i][j])/(1+abs(h[i][j]))))
        extra[family]={'score_error':ms,'observed_error':mh,'points':len(xs)}
        if ms>=2e-6 or mh>=2e-5:raise ValueError(f'{family}: tolerance exceeded')
    results['conditional_chart_checks']={'status':'pass','reason':'','checks':extra}
except Exception as exc:
    results['conditional_chart_checks']={'status':'fail','reason':f'{type(exc).__name__}: {exc}'}
# Additional symmetric skew-GED submodels at the join. These are numerical
# reference points, not claims that the unrestricted golden fit held xi fixed.
try:
    fixture=json.loads((DIST/'sged/golden.json').read_text())
    extra={}
    for label,p,free in [('normal',[0.,1.,2.,1.],[0,1]),('held_location',[0.,1.,1.5,1.],[1,2])]:
        mask=sum(1<<i for i in free);maxs=maxh=0.
        for x in [-1.,-.1,0.,.1,1.]:
            v=run('sged',x,p,mask);n=len(free)
            if int(v[0])!=0 or int(v[1])!=n:raise ValueError(f'{label}: production status {v[0]}')
            gs,hs=direct_derivatives('sged',x,p,free)
            for i in range(n):
                maxs=max(maxs,float(abs(v[2+i]-gs[i])/(1+abs(gs[i]))))
                for j in range(n):
                    k=i*n+j;maxh=max(maxh,float(abs(v[2+n+k]-hs[i][j])/(1+abs(hs[i][j]))))
        extra[label]={'points':5,'max_score_error':maxs,'max_observed_error':maxh}
        if maxs>=2e-6 or maxh>=2e-5:raise ValueError(f'{label}: tolerance exceeded: {extra[label]}')
    results['sged']['additional_submodels']=extra
except Exception as exc:
    results['sged'].update(status='fail',reason=f'additional symmetric submodel: {type(exc).__name__}: {exc}')
# Boundary values held as known constants and finite-support rank checks.
# References below are exact reduced normal, Bernoulli and Poisson models.
try:
    def reduced(label,x,p):
        if label in ('bhatt','sn'):
            mu,sd,_=p;return -mp.log(sd)-mp.log(2*mp.pi)/2-((x-mu)/sd)**2/2
        if label=='foldnorm':
            _,sd=p;return mp.log(2)-mp.log(sd)-mp.log(2*mp.pi)/2-(x/sd)**2/2
        if label=='zip':
            lam,_=p;return x*mp.log(lam)-lam-mp.loggamma(x+1)
        if label=='zmpois':
            lam,_=p;return x*mp.log(lam)-lam-mp.loggamma(x+1)-mp.log1p(-mp.exp(-lam))
        if label=='bbinom':
            _,a,b=p;q=a/(a+b);return x*mp.log(q)+(1-x)*mp.log1p(-q)
        if label=='cmb':
            _,q,_=p;return x*mp.log(q)+(1-x)*mp.log1p(-q)
        if label=='zmbinom':
            _,_,p0=p;return mp.log(p0) if x==0 else mp.log1p(-p0)
        raise ValueError(label)
    specifications=[('bhatt',[0.,1.,0.],[0,1],[-1.,0.,1.]),
      ('sn',[0.,1.,0.],[0,1],[-1.,0.,1.]),('foldnorm',[0.,1.],[1],[0.,1.,2.]),
      ('zip',[2.,0.],[0],[0.,1.,2.]),('zmpois',[2.,0.],[0],[1.,2.,3.]),
      ('bbinom',[1.,2.,3.],[1],[0.,1.]),('cmb',[1.,.4,.8],[1],[0.,1.]),
      ('zmbinom',[1.,.4,.2],[2],[0.,1.])]
    checks={}
    for family,p,free,xs in specifications:
        mask=sum(1<<i for i in free);n=len(free);ms=mh=0.
        for x in xs:
            v=run(family,x,p,mask)
            if int(v[0])!=0 or int(v[1])!=n: raise ValueError(f'{family}: status {v[0]}')
            values=list(map(mp.mpf,p));coordinates=tuple(values[i] for i in free)
            def ref(*q):
                c=values.copy()
                for i,z in zip(free,q):c[i]=z
                return reduced(family,mp.mpf(x),c)
            for i in range(n):
                order=[0]*n;order[i]=1;g=mp.diff(ref,coordinates,tuple(order))
                ms=max(ms,float(abs(v[2+i]-g)/(1+abs(g))))
                for j in range(n):
                    order=[0]*n;order[i]+=1;order[j]+=1;h=-mp.diff(ref,coordinates,tuple(order))
                    mh=max(mh,float(abs(v[2+n+i*n+j]-h)/(1+abs(h))))
        checks[family]={'score_error':ms,'observed_error':mh,'points':len(xs)}
        if ms>=2e-6 or mh>=2e-5:raise ValueError(f'{family}: tolerance exceeded')
    singular=[('bnbinom',[2.,3.,2.],5,1.),('bnbinom',[2.,3.,2.],7,1.),
              ('genbeta',[2.,1.,3.,1.],5,.4),('genbeta',[2.,1.,3.,1.],7,.4),
              ('bbinom',[1.,2.,3.],6,1.),('bbinom',[0.,2.,3.],6,0.),
              ('zib',[1.,.4,.2],6,1.),('zib',[0.,.4,.2],6,0.),('ztbinom',[1.,.4],2,1.),
              ('zipf',[1.,2.],2,1.),('cmb',[1.,.4,.8],4,1.),('cmb',[0.,.4,.8],6,0.),
              ('zmbinom',[1.,.4,.2],2,1.),('tpois',[2.,0.,1.],1,1.),
              ('tbinom',[.4,0.,1.,4.],1,1.),('zip',[2.,1.],1,0.),('sn',[0.,1.,0.],5,0.),
              ('zmpois',[2.,1.],1,0.),('zmbinom',[4.,.4,1.],2,0.),
              ('nbinom',[0.,.4],2,0.),('zinb',[0.,.4,.2],6,0.),('zib',[4.,0.,.2],4,0.),
              ('zmnbinom',[2.,1.,.3],1,1.),('ztnbinom',[2.,1.],1,1.)]
    for family,p,mask,x in singular:
        status=int(run(family,x,p,mask)[0])
        if status!=6:raise ValueError(f'{family}: expected Singular, got {status}')
    results['known_boundary_and_rank']={'status':'pass','reason':'','checks':checks,'singular_cases':len(singular)}
except Exception as exc:
    results['known_boundary_and_rank']={'status':'fail','reason':f'{type(exc).__name__}: {exc}'}
# More known-boundary identities: geometric, exponential, Bernoulli, and
# logarithmic-series submodels. Only the genuinely free subset is perturbed.
try:
    def boundary_reference(label,x,p):
        if label=='cmp':
            lam,_=p;return mp.log1p(-lam)+x*mp.log(lam)
        if label=='sgomp':
            rate,_=p;return mp.log(rate)-rate*x
        if label in ('zmpois','zmgeom','zmlogarithmic'):
            _,p0=p;return mp.log(p0) if x==0 else mp.log1p(-p0)
        if label=='zmbinom':
            _,_,p0=p;return mp.log(p0) if x==0 else mp.log1p(-p0)
        if label=='zib':
            _,_,pi=p;return mp.log(pi) if x==0 else mp.log1p(-pi)
        if label=='zmnbinom':
            _,prob,p0=p
            return mp.log(p0) if x==0 else mp.log1p(-p0)+x*mp.log1p(-prob)-mp.log(x)-mp.log(-mp.log(prob))
        if label=='ztnbinom':
            _,prob=p;return x*mp.log1p(-prob)-mp.log(x)-mp.log(-mp.log(prob))
        raise ValueError(label)
    specifications=[('cmp',[.3,0.],[0],[0.,1.,2.]),('sgomp',[2.,0.],[0],[.2,.5,1.]),
       ('zmpois',[0.,.25],[1],[0.,1.]),('zmgeom',[1.,.25],[1],[0.,1.]),
       ('zmlogarithmic',[0.,.25],[1],[0.,1.]),('zmbinom',[4.,0.,.25],[2],[0.,1.]),
       ('zib',[4.,1.,.25],[2],[0.,4.]),('zmnbinom',[0.,.4,.3],[1,2],[0.,1.,2.,3.]),
       ('ztnbinom',[0.,.4],[1],[1.,2.,3.])]
    checks={}
    for family,p,free,xs in specifications:
        mask=sum(1<<i for i in free);n=len(free);ms=mh=0.
        for x in xs:
            v=run(family,x,p,mask)
            if int(v[0])!=0 or int(v[1])!=n:raise ValueError(f'{family}: status {v[0]}')
            values=list(map(mp.mpf,p));coordinates=tuple(values[i] for i in free)
            def ref(*q):
                c=values.copy()
                for i,z in zip(free,q):c[i]=z
                return boundary_reference(family,mp.mpf(x),c)
            for i in range(n):
                order=[0]*n;order[i]=1;g=mp.diff(ref,coordinates,tuple(order))
                ms=max(ms,float(abs(v[2+i]-g)/(1+abs(g))))
                for j in range(n):
                    order=[0]*n;order[i]+=1;order[j]+=1;h=-mp.diff(ref,coordinates,tuple(order))
                    mh=max(mh,float(abs(v[2+n+i*n+j]-h)/(1+abs(h))))
        checks[family]={'score_error':ms,'observed_error':mh,'points':len(xs)}
        if ms>=2e-6 or mh>=2e-5:raise ValueError(f'{family}: tolerance exceeded')
    results['more_known_boundary_submodels']={'status':'pass','reason':'','checks':checks}
except Exception as exc:
    results['more_known_boundary_submodels']={'status':'fail','reason':f'{type(exc).__name__}: {exc}'}
paths=[Path(__file__),INC/'core/inference_numeric.hpp',INC/'core/inference_families.hpp',INC/'core/inference_numeric_special.hpp',INC/'core/inference_numeric_generated.hpp']
paths += [DIST/f/name for f in sorted(set(FAMILIES)|{'bhatt','zip','zmpois','bbinom','zib','ztbinom','zipf','sgomp','zmgeom','zmlogarithmic','zmnbinom','ztnbinom','sn','foldnorm','nbinom','zinb'}) for name in ('README.md','dpqr.hpp','golden.json')]
result={'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        'source_sha256':{str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in paths},
        'seed':SEED,'mpmath_dps':mp.mp.dps,'tolerance_score':2e-6,'tolerance_observed':2e-5,
        'golden_total_score_tolerance':1e-3,'errors_scaled_by':'1 + abs(reference)','families':results}
(OUT/'results.json').write_text(json.dumps(result,indent=2)+'\n')
if any(x['status']!='pass' for x in results.values()):raise SystemExit('special validator has failures or open scopes; see results.json')
