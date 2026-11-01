#!/usr/bin/env python3
"""Independent registry derivative checks at seeded points and golden fits.

Writes structured outcomes, including oracle and regime gaps, under scratch.
Scaled error is abs(actual-reference)/(1+abs(reference)). A run with unresolved
families or tolerance failures exits nonzero; skipped scopes are not passes.
"""
import json
import hashlib
import math
import os
from pathlib import Path
import random
import re
import subprocess
import sys
import shutil
import mpmath as mp
import sympy as sp
import yaml

ROOT=Path(__file__).resolve().parents[2]
os.environ.setdefault('CCACHE_DIR',str(Path.home()/'.cache/ccache/univariateML'))
os.environ.setdefault('CCACHE_BASEDIR',str(ROOT))
os.environ.setdefault('CCACHE_NOHASHDIR','1')
COMPILER=([shutil.which('ccache')] if shutil.which('ccache') else [])+[os.environ.get('CXX','c++')]

INC=ROOT/'univariateML-cpp/include/cppunivariate'
DIST=INC/'distributions'
OUT=ROOT/'build/scratch/inference-numeric-registry'
OUT.mkdir(parents=True,exist_ok=True)
registry=(INC/'core/inference_numeric_generated.hpp').read_text()
FAMILIES=re.findall(r'#include "\.\./distributions/([^/]+)/dpqr.hpp"',registry)
SEED=20261101
rng=random.Random(SEED)
mp.mp.dps=40
source='''#include <iostream>
#include <iomanip>
#include <vector>
#include "cppunivariate/core/inference_families.hpp"
int main() {std::cout<<std::setprecision(17);std::string f;std::size_t count;unsigned mask;
while(std::cin>>f>>count>>mask) {
std::array<double,cppunivariate::inference_max_parameters> p{};for(auto& v:p)std::cin>>v;
std::vector<double> d(count);for(auto& x:d)std::cin>>x;
const auto r=cppunivariate::family_inference({d.data(),d.size()},f,p,mask);
std::cout<<static_cast<int>(r.status)<<" "<<r.dimension;
for(std::size_t i=0;i<r.dimension;++i)std::cout<<" "<<r.score[i];
for(std::size_t i=0;i<r.dimension*r.dimension;++i)std::cout<<" "<<r.observed[i];
std::cout<<"\\n";
}}
'''
(OUT/'probe.cpp').write_text(source)
boost=os.environ.get('BOOST_INCLUDE_DIR','')
cache=ROOT/'univariateML-cpp/build/CMakeCache.txt'
if not boost and cache.exists():
    boost=next((line.split('=',1)[1] for line in cache.read_text().splitlines() if line.startswith('Boost_INCLUDE_DIR:')),'')
subprocess.run(COMPILER+['-std=c++17','-O2','-I'+boost,
                '-I'+str(INC.parent),str(OUT/'probe.cpp'),'-o',str(OUT/'probe')],check=True)
results={}
for family in FAMILIES:
    item={'status':'open','reason':'not evaluated'};results[family]=item
    try:
        meta=yaml.safe_load((DIST/family/'README.md').read_text().split('---',2)[1])
        golden=json.loads((DIST/family/'golden.json').read_text())
        params=meta['params'];names=[p['name'] for p in params]
        values=[golden['estimates'][name] for name in names]
        fit_args=golden.get('fit_args') or {}
        known={name for name,val in fit_args.items() if name in names and val is not None}
        structural=[p['name'] for p in params if p.get('kind','regular')!='regular' or (family=='dlaplace' and p['name']=='location')]
        fitted_structural=[name for name in structural if name in fit_args and fit_args[name] is None]
        free=[i for i,p in enumerate(params) if p['name'] not in structural and p['name'] not in known]
        mask=sum(1<<i for i in free);n=len(free)
        if not n: raise ValueError('no continuous free coefficient in scope')
        # Probe the complete golden sample plus five reproducibly selected data
        # points at a nearby interior parameter vector.
        perturbed=values.copy()
        for i in free:
            domain=str(params[i]['domain']).replace(' ','')
            if domain in ('(-inf,inf)','(-inf,inf]'): perturbed[i]+=0.1
            elif '1' in domain and domain in ('(0,1)','(0,1]','[0,1]','[0,1)'):
                p=values[i]; perturbed[i]=1/(1+math.exp(-(math.log(p/(1-p))+.05)))
            elif domain=='(2,inf)': perturbed[i]=2+1.05*(values[i]-2)
            elif domain in ('(1,inf)','[1,inf)'): perturbed[i]=1+1.05*(values[i]-1)
            else: perturbed[i]=1.05*values[i]
        cases=[(x,values,'golden') for x in golden['data']]
        cases.extend((rng.choice(golden['data']),perturbed,'seeded') for _ in range(5))
        def fmt(f,xs,p):
            q=p+[0.]*(8-len(p))
            return f'{f} {len(xs)} {mask} '+ ' '.join(f'{v:.17g}' for v in q)+' '+ ' '.join(f'{x:.17g}' for x in xs)+'\n'
        request=''.join(fmt(family,[x],p) for x,p,_ in cases)+fmt(family,golden['data'],values)
        process=subprocess.run([str(OUT/'probe')],input=request,text=True,capture_output=True,check=True)
        lines=process.stdout.splitlines()
        if len(lines)!=len(cases)+1: raise ValueError('probe output count mismatch')
        aggregate=list(map(float,lines.pop().split()))
        if int(aggregate[0])!=0 or int(aggregate[1])!=n: raise ValueError(f'full-data production status {aggregate[0]}')
        syms=[sp.Symbol('param_'+name,real=True) for name in names];sx=sp.Symbol('x',real=True)
        local=dict(zip(names,syms),x=sx)
        text=meta['log_density']
        if 'lambda' in names:
            text=re.sub(r'\blambda\b','lambda_parameter',text)
            local['lambda_parameter']=syms[names.index('lambda')]
        if family in ('dgamma','slash','zipf'):
            # Direct high-precision densities avoid symbolic singular expressions
            # for incomplete-gamma endpoints, Abs derivatives, and finite sums.
            def direct(x,*a):
                if family=='dgamma':
                    shape,rate=a
                    return mp.log(mp.gammainc(shape,rate*x,rate*(x+1)))-mp.loggamma(shape)
                if family=='slash':
                    mu,sigma=a; z=(x-mu)/sigma
                    # Integral of u exp(-z^2 u^2/2), evaluated stably.
                    kernel=-mp.log(2) if z==0 else mp.log(-mp.expm1(-z*z/2))-2*mp.log(abs(z))
                    return -mp.log(sigma)-mp.log(2*mp.pi)/2+kernel
                size,exponent=a
                return -exponent*mp.log(x)-mp.log(mp.fsum(mp.power(k,-exponent) for k in range(1,int(size)+1)))
            def derivative(x,p,orders):
                return mp.diff(lambda *v: direct(x,*v),tuple(p),tuple(orders))
            def grad_at(i,args):
                orders=[0]*len(names);orders[free[i]]=1
                return derivative(args[0],args[1:],orders)
            def hess_at(i,j,args):
                orders=[0]*len(names);orders[free[i]]+=1;orders[free[j]]+=1
                return -derivative(args[0],args[1:],orders)
        else:
            expr=sp.sympify(text,locals=local)
            grad=[sp.lambdify([sx,*syms],sp.diff(expr,syms[a]),'mpmath') for a in free]
            hess=[[sp.lambdify([sx,*syms],-sp.diff(expr,syms[a],syms[b]),'mpmath') for b in free] for a in free]
            def grad_at(i,args): return grad[i](*args)
            def hess_at(i,j,args): return hess[i][j](*args)
        maxscore=maxh=0.;total=[0.]*n;ref_total=[mp.mpf('0')]*n
        totalh=[0.]*(n*n);refh=[mp.mpf('0')]*(n*n)
        for (x,p,kind),line in zip(cases,lines):
            v=list(map(float,line.split()));status,dimension=int(v[0]),int(v[1])
            if status!=0 or dimension!=n: raise ValueError(f'production status {status}, dimension {dimension}, expected {n}, case {kind}')
            args=[mp.mpf(x),*map(mp.mpf,p)]
            for i in range(n):
                reference=grad_at(i,args);actual=v[2+i]
                err=float(abs(actual-reference)/(1+abs(reference)))
                if not math.isfinite(err): raise ValueError('nonfinite score reference or error')
                maxscore=max(maxscore,err)
                if kind=='golden': total[i]+=actual;ref_total[i]+=reference
                for j in range(n):
                    k=i*n+j;reference=hess_at(i,j,args);actual=v[2+n+k]
                    err=float(abs(actual-reference)/(1+abs(reference)))
                    if not math.isfinite(err): raise ValueError('nonfinite Hessian reference or error')
                    maxh=max(maxh,err)
                    if kind=='golden': totalh[k]+=actual;refh[k]+=reference
        # Compare the actual full-data call, not merely a sum of one-row calls.
        total=aggregate[2:2+n];totalh=aggregate[2+n:2+n+n*n]
        total_score_err=max(float(abs(a-b)/(1+abs(b))) for a,b in zip(total,ref_total))
        total_h_err=max(float(abs(a-b)/(1+abs(b))) for a,b in zip(totalh,refh))
        item.update(status='pass',reason='',n=len(golden['data']),seeded=5,free=[names[i] for i in free],
                    max_score_error=maxscore,max_observed_error=maxh,total_score_error=total_score_err,
                    held_regular=sorted(known.intersection(names)),
                    conditioned_on_fitted_structural=fitted_structural,
                    total_observed_error=total_h_err,golden_total_score=total)
        if maxscore>=2e-6 or maxh>=2e-5 or total_score_err>=2e-6 or total_h_err>=2e-5 or max(map(abs,total))>=1e-3:
            item.update(status='fail',reason='scaled-error or golden-score tolerance exceeded')
    except Exception as exc:
        item.update(status='open',reason=f'{type(exc).__name__}: {exc}')
    print(f'{family}: {item["status"]} {item["reason"]}',flush=True)
source_paths=[Path(__file__),INC/'core/inference_numeric.hpp',INC/'core/inference_families.hpp',
              INC/'core/inference_numeric_generated.hpp',INC/'core/inference_numeric_special.hpp']
source_paths += [DIST/f/name for f in FAMILIES for name in ('README.md','dpqr.hpp','golden.json')]
source_hashes={str(path.relative_to(ROOT)):hashlib.sha256(path.read_bytes()).hexdigest() for path in source_paths}
result={'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        'source_sha256':source_hashes,'python':sys.version,'sympy':sp.__version__,'mpmath':mp.__version__,
        'seed':SEED,'mpmath_dps':mp.mp.dps,'tolerance_score':2e-6,'tolerance_observed':2e-5,
        'golden_total_score_tolerance':1e-3,'errors_scaled_by':'1 + abs(reference)','families':results}
(OUT/'results.json').write_text(json.dumps(result,indent=2)+'\n')
if any(r['status']!='pass' for r in results.values()):
    raise SystemExit('registry validator has failures or open scopes; see results.json')
