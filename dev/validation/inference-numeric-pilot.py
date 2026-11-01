#!/usr/bin/env python3
"""Independent pilot, not the requested full family validator.

Compares the experimental numeric derivatives with differentiated README
log densities at seeded points and each pilot golden dataset.  Run under a
CPU slot; all generated files remain in build/scratch.
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

ROOT = Path(__file__).resolve().parents[2]
os.environ.setdefault('CCACHE_DIR',str(Path.home()/'.cache/ccache/univariateML'))
os.environ.setdefault('CCACHE_BASEDIR',str(ROOT))
os.environ.setdefault('CCACHE_NOHASHDIR','1')
COMPILER=([shutil.which('ccache')] if shutil.which('ccache') else [])+[os.environ.get('CXX','c++')]

OUT = ROOT / 'build/scratch/inference-numeric-pilot'
OUT.mkdir(parents=True, exist_ok=True)
DIST = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
FAMILIES = {'norm': ['mean', 'sd'], 'gamma': ['shape', 'scale'],
            'beta': ['shape1', 'shape2'], 'pois': ['lambda']}
SEED = 20261101
rng = random.Random(SEED)
mp.mp.dps = 40
source = '''#include <iostream>
#include <iomanip>
#include <vector>
#include "cppunivariate/core/inference_numeric.hpp"
'''
source += ''.join(f'#include "cppunivariate/distributions/{f}/dpqr.hpp"\n' for f in FAMILIES)
source += '''int main() {
std::cout<<std::setprecision(17); std::string f; double x,a,b;
while(std::cin>>f>>x>>a>>b) {
std::array<cppunivariate::NumericParameter,cppunivariate::inference_max_parameters> p{};
p[0]={"a",a,f=="norm"?cppunivariate::NumericCoordinate::Real:cppunivariate::NumericCoordinate::Positive,0};
p[1]={"b",b,cppunivariate::NumericCoordinate::Positive,0};
auto density=[&](double y,const auto& v) {
'''
for f, names in FAMILIES.items():
    args = 'v[0]' if len(names) == 1 else 'v[0],v[1]'
    source += f'if(f=="{f}") return cppunivariate::distributions::d{f}(y,{args},true);\n'
source += '''return static_cast<double>(NAN);};
std::vector<double> d{x}; const std::size_t n=f=="pois"?1:2;
const auto r=cppunivariate::numeric_inference({d.data(),d.size()},p,n,(1u<<n)-1,density);
std::cout<<static_cast<int>(r.status); for(std::size_t i=0;i<n;++i) std::cout<<" "<<r.score[i];
for(std::size_t i=0;i<n*n;++i) std::cout<<" "<<r.observed[i]; std::cout<<"\\n";
}}
'''
(OUT/'probe.cpp').write_text(source)
cache = ROOT/'univariateML-cpp/build/CMakeCache.txt'
boost = os.environ.get('BOOST_INCLUDE_DIR', '')
if not boost and cache.exists():
    boost = next((line.split('=',1)[1] for line in cache.read_text().splitlines()
                  if line.startswith('Boost_INCLUDE_DIR:')), '')
subprocess.run(COMPILER+['-std=c++17','-O2',
                '-I'+boost,'-I'+str(ROOT/'univariateML-cpp/include'),
                str(OUT/'probe.cpp'),'-o',str(OUT/'probe')],check=True)
points = []
for family,names in FAMILIES.items():
    for _ in range(16):
        p = [rng.uniform(-2,2),rng.uniform(.7,2.5)] if family=='norm' else [rng.uniform(1.2,5) for _ in names]
        x = rng.uniform(-3,3) if family=='norm' else rng.uniform(.08,.92) if family=='beta' else rng.randrange(12) if family=='pois' else rng.uniform(.2,8)
        points.append((family,x,p,'seeded'))
    golden = json.loads((DIST/family/'golden.json').read_text())
    p = [golden['estimates'][name] for name in names]
    points += [(family,x,p,'golden') for x in golden['data']]
input_text = ''.join(f'{f} {x:.17g} {p[0]:.17g} {(p[1] if len(p)>1 else 0):.17g}\n' for f,x,p,_ in points)
lines = subprocess.run([str(OUT/'probe')],input=input_text,text=True,capture_output=True,check=True).stdout.splitlines()
assert len(lines)==len(points)
compiled = {}
errors = {f: {'score':0.,'observed':0.} for f in FAMILIES}
golden_scores = {f:[0.]*len(names) for f,names in FAMILIES.items()}
for (family,x,p,kind),line in zip(points,lines):
    names=FAMILIES[family]; n=len(names); v=list(map(float,line.split()))
    assert v[0]==0, (family,x,p,v)
    if family not in compiled:
        front=yaml.safe_load((DIST/family/'README.md').read_text().split('---')[1])
        symbols=[sp.Symbol(name,positive=True) for name in names]
        sx=sp.Symbol('x',positive=True)
        local=dict(zip(names,symbols),x=sx)
        if family=='pois': local['poisson_lambda']=symbols[0]
        expr=sp.sympify(front['log_density'].replace('lambda','poisson_lambda'),locals=local)
        compiled[family]=([sp.lambdify([sx,*symbols],sp.diff(expr,a),'mpmath') for a in symbols],
          [[sp.lambdify([sx,*symbols],-sp.diff(expr,a,b),'mpmath') for b in symbols] for a in symbols])
    score,observed=compiled[family]; args=[mp.mpf(x),*map(mp.mpf,p)]
    for i in range(n):
        ref=score[i](*args); err=float(abs(v[1+i]-ref)/(1+abs(ref)))
        errors[family]['score']=max(errors[family]['score'],err)
        if kind=='golden': golden_scores[family][i]+=v[1+i]
        for j in range(n):
            ref=observed[i][j](*args); err=float(abs(v[1+n+i*n+j]-ref)/(1+abs(ref)))
            errors[family]['observed']=max(errors[family]['observed'],err)
for family in FAMILIES:
    assert errors[family]['score']<2e-6,(family,errors[family])
    assert errors[family]['observed']<2e-5,(family,errors[family])
    assert max(map(abs,golden_scores[family]))<1e-3,(family,golden_scores[family])
result={'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        'seed':SEED,'points':len(points),'mpmath_dps':mp.mp.dps,
        'scaled_error_denominator':'1 + abs(reference)',
        'tolerance':{'score':2e-6,'observed':2e-5},
        'maximum_errors':errors,'golden_total_scores':golden_scores}
(OUT/'results.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
