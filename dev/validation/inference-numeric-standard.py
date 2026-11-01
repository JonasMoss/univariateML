#!/usr/bin/env python3
"""Seeded and golden derivative comparisons for the current standard-model dispatch.

The binomial check conditions on golden size; it does not validate a jointly
estimated integer size. Scaled error is abs(actual-reference)/(1+abs(reference)).
Run under a shared CPU slot; scratch output stays in the worktree.
"""
import json
import os
from pathlib import Path
import random
import subprocess
import shutil
import mpmath as mp
import sympy as sp
import yaml

ROOT=Path(__file__).resolve().parents[2]
os.environ.setdefault('CCACHE_DIR',str(Path.home()/'.cache/ccache/univariateML'))
os.environ.setdefault('CCACHE_BASEDIR',str(ROOT))
os.environ.setdefault('CCACHE_NOHASHDIR','1')
COMPILER=([shutil.which('ccache')] if shutil.which('ccache') else [])+[os.environ.get('CXX','c++')]

DIST=ROOT/'univariateML-cpp/include/cppunivariate/distributions'
OUT=ROOT/'build/scratch/inference-numeric-standard'
OUT.mkdir(parents=True,exist_ok=True)
FAMILIES={'norm':['mean','sd'],'gamma':['shape','scale'],'beta':['shape1','shape2'],
          'pois':['lambda'],'exp':['rate'],'geom':['prob'],'lnorm':['meanlog','sdlog'],
          'logitnorm':['mu','sigma'],'lgamma':['shapelog','ratelog'],
          'rayleigh':['sigma'],'binom':['size','prob']}
SEED=20261101
rng=random.Random(SEED)
mp.mp.dps=40
source='''#include <iostream>
#include <iomanip>
#include <vector>
#include "cppunivariate/core/inference_families.hpp"
int main() {std::cout<<std::setprecision(17);std::string f;double a,b;std::size_t count;
while(std::cin>>f>>count>>a>>b) {
std::array<double,cppunivariate::inference_max_parameters> p{};p[0]=a;p[1]=b;
std::vector<double> d(count);for(auto& x:d) std::cin>>x;
unsigned mask=f=="binom"?2:(f=="norm"||f=="gamma"||f=="beta"||f=="lnorm"||f=="logitnorm"||f=="lgamma"?3:1);
const auto r=cppunivariate::family_inference({d.data(),d.size()},f,p,mask);
std::cout<<static_cast<int>(r.status)<<" "<<r.dimension;
for(std::size_t i=0;i<r.dimension;++i) std::cout<<" "<<r.score[i];
for(std::size_t i=0;i<r.dimension*r.dimension;++i) std::cout<<" "<<r.observed[i];
std::cout<<"\\n";
}}
'''
(OUT/'probe.cpp').write_text(source)
boost=os.environ.get('BOOST_INCLUDE_DIR','')
cache=ROOT/'univariateML-cpp/build/CMakeCache.txt'
if not boost and cache.exists():
    boost=next((line.split('=',1)[1] for line in cache.read_text().splitlines() if line.startswith('Boost_INCLUDE_DIR:')),'')
subprocess.run(COMPILER+['-std=c++17','-O2','-I'+boost,
                '-I'+str(ROOT/'univariateML-cpp/include'),str(OUT/'probe.cpp'),'-o',str(OUT/'probe')],check=True)
points=[]
goldens={}
for family,names in FAMILIES.items():
    for _ in range(16):
        if family in ('norm','lnorm','logitnorm'): p=[rng.uniform(-2,2),rng.uniform(.7,2.5)]
        elif family=='binom': p=[10,rng.uniform(.15,.85)]
        elif family=='geom': p=[rng.uniform(.15,.85)]
        else: p=[rng.uniform(1.2,5) for _ in names]
        if family=='norm': x=rng.uniform(-3,3)
        elif family in ('beta','logitnorm'): x=rng.uniform(.08,.92)
        elif family in ('pois','geom','binom'): x=rng.randrange(10)
        elif family=='lgamma': x=rng.uniform(1.1,8)
        else: x=rng.uniform(.2,8)
        points.append((family,x,p,'seeded'))
    golden=json.loads((DIST/family/'golden.json').read_text())
    p=[golden['estimates'][name] for name in names]
    goldens[family]=(golden['data'],p)
    points.extend((family,x,p,'golden') for x in golden['data'])
def request(f,xs,p):
    return f'{f} {len(xs)} {p[0]:.17g} {(p[1] if len(p)>1 else 0):.17g} '+' '.join(f'{x:.17g}' for x in xs)+'\n'
input_text=''.join(request(f,[x],p) for f,x,p,_ in points)
input_text+=''.join(request(f,xs,p) for f,(xs,p) in goldens.items())
lines=subprocess.run([str(OUT/'probe')],input=input_text,text=True,capture_output=True,check=True).stdout.splitlines()
assert len(lines)==len(points)+len(FAMILIES)
aggregate=dict(zip(FAMILIES,lines[len(points):]));lines=lines[:len(points)]
compiled={}
errors={f:{'score':0.,'observed':0.} for f in FAMILIES}
total={f:[0.]*(1 if f=='binom' else len(names)) for f,names in FAMILIES.items()}
refscore={f:[mp.mpf('0')]*len(total[f]) for f in FAMILIES}
refh={f:[mp.mpf('0')]*(len(total[f])**2) for f in FAMILIES}
for (family,x,p,kind),line in zip(points,lines):
    names=FAMILIES[family]; free=[1] if family=='binom' else list(range(len(names))); n=len(free)
    v=list(map(float,line.split()));assert v[0]==0 and v[1]==n,(family,x,p,v)
    if family not in compiled:
        front=yaml.safe_load((DIST/family/'README.md').read_text().split('---')[1])
        symbols=[sp.Symbol(name,positive=True) for name in names];sx=sp.Symbol('x',positive=True)
        local=dict(zip(names,symbols),x=sx)
        if family=='pois': local['poisson_lambda']=symbols[0]
        expr=sp.sympify(front['log_density'].replace('lambda','poisson_lambda'),locals=local)
        compiled[family]=([sp.lambdify([sx,*symbols],sp.diff(expr,a),'mpmath') for a in symbols],
          [[sp.lambdify([sx,*symbols],-sp.diff(expr,a,b),'mpmath') for b in symbols] for a in symbols])
    score,observed=compiled[family];args=[mp.mpf(x),*map(mp.mpf,p)]
    for i,a in enumerate(free):
        ref=score[a](*args);err=float(abs(v[2+i]-ref)/(1+abs(ref)))
        errors[family]['score']=max(errors[family]['score'],err)
        if kind=='golden': refscore[family][i]+=ref
        for j,b in enumerate(free):
            ref=observed[a][b](*args);err=float(abs(v[2+n+i*n+j]-ref)/(1+abs(ref)))
            errors[family]['observed']=max(errors[family]['observed'],err)
            if kind=='golden': refh[family][i*n+j]+=ref
for family in FAMILIES:
    n=len(total[family]);v=list(map(float,aggregate[family].split()))
    assert v[0]==0 and v[1]==n,(family,'full data status',v[:2])
    total[family]=v[2:2+n]
    observed=v[2+n:2+n+n*n]
    errors[family]['total_score']=max(float(abs(a-b)/(1+abs(b))) for a,b in zip(total[family],refscore[family]))
    errors[family]['total_observed']=max(float(abs(a-b)/(1+abs(b))) for a,b in zip(observed,refh[family]))
    assert errors[family]['total_score']<2e-6,(family,errors[family])
    assert errors[family]['total_observed']<2e-5,(family,errors[family])
    assert errors[family]['score']<2e-6,(family,errors[family])
    assert errors[family]['observed']<2e-5,(family,errors[family])
    assert max(map(abs,total[family]))<1e-3,(family,total[family])
result={'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        'seed':SEED,'points':len(points),'mpmath_dps':mp.mp.dps,'scaled_error_denominator':'1 + abs(reference)',
        'tolerance':{'score':2e-6,'observed':2e-5},'maximum_errors':errors,'golden_total_scores':total,
        'binom_scope':'size conditioned on its golden estimate; no joint discrete-size inference'}
(OUT/'results.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
