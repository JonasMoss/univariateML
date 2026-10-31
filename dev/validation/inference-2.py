#!/usr/bin/env python3
"""Independent seeded README differentiation/expectation checks of production C++.
Run from repository root under the CPU slot lock. Requires existing SymPy,
mpmath and PyYAML. Writes only build/scratch/inference-2; installs nothing.
"""
import hashlib
import json
import os
from pathlib import Path
import platform
import random
import subprocess
import sys
import mpmath as mp
import sympy as sp
import yaml

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'build/scratch/inference-2'
OUT.mkdir(parents=True, exist_ok=True)
mp.mp.dps = 45
SEED = 20261031
rng = random.Random(SEED)
families = {'norm': ['mean','sd'], 'gamma':['shape','scale'],
            'beta':['shape1','shape2'], 'pois':['lambda']}
# Tiny stdin/stdout executable uses the actual public API, not copied formulas.
source = '#include <iostream>\n#include <iomanip>\n'
source += ''.join(f'#include "cppunivariate/distributions/{f}/score.hpp"\n' for f in families)
source += '''int main() { std::cout<<std::setprecision(17);
std::string f; double x,a,b; unsigned mask;
while(std::cin>>f>>x>>a>>b>>mask) {
std::vector<double> data{x}; cppunivariate::InferenceResult r;
'''
for f,names in families.items():
    args = 'a,b' if len(names)==2 else 'a'
    source += f'if(f=="{f}") r=cppunivariate::{f}_inference({{data.data(),1}},{args},mask);\n'
source += '''std::cout<<static_cast<int>(r.status)<<" "<<r.dimension;
for(std::size_t j=0;j<r.dimension;++j) std::cout<<" "<<r.score[j];
for(auto m:{r.observed,r.fisher,r.opg}) for(std::size_t j=0;j<r.dimension*r.dimension;++j) std::cout<<" "<<m[j];
std::cout<<"\\n";
}}
'''
(OUT/'probe.cpp').write_text(source)
# Use an explicit Boost directory or the already configured CMake header
# path. This is a plain include path; no R headers or linkage are involved.
boost = os.environ.get('Boost_INCLUDE_DIR', os.environ.get('BOOST_INCLUDE_DIR', ''))
cache = ROOT/'univariateML-cpp/build/CMakeCache.txt'
if not boost and cache.exists():
    for line in cache.read_text().splitlines():
        if line.startswith('Boost_INCLUDE_DIR:'):
            boost = line.split('=',1)[1]
flags = ['-I'+boost] if boost else []
subprocess.run([os.environ.get('CXX','c++'), '-std=c++17','-O2', *flags,
                '-I'+str(ROOT/'univariateML-cpp/include'), str(OUT/'probe.cpp'),
                '-o',str(OUT/'probe')],check=True)
points=[]
for f,names in families.items():
    for i in range(8):
        p=[rng.uniform(-2,2),rng.uniform(.7,2.5)] if f=='norm' else [rng.uniform(1.2,5) for _ in names]
        x=rng.uniform(-3,3) if f=='norm' else rng.uniform(.08,.92) if f=='beta' else rng.randrange(0,12) if f=='pois' else rng.uniform(.2,8)
        for mask in range(1<<len(names)):
            points.append((f,x,p,mask))
stdin=''.join(f'{f} {x:.17g} {p[0]:.17g} {(p[1] if len(p)>1 else 0):.17g} {mask}\n' for f,x,p,mask in points)
lines=subprocess.run([str(OUT/'probe')],input=stdin,text=True,capture_output=True,check=True).stdout.splitlines()
assert len(lines)==len(points)
maxerr={'score':0.,'hessian_symbolic':0.,'hessian_central':0.,'opg':0.,'fisher_expectation':0.,'transform':0.}
def compare(label,a,b,tol):
    err=float(abs(a-b)/(1+abs(b)))
    maxerr[label]=max(maxerr[label],err)
    assert err<tol,(label,a,b,err,tol)
compiled={}
fisher_seen=set()
for (f,x,p,mask),line in zip(points,lines):
    names=families[f]; n=len(names); free=[j for j in range(n) if mask&(1<<j)]
    vals=list(map(float,line.split())); status,dim=map(int,vals[:2]); assert status==0 and dim==len(free)
    s=vals[2:2+dim]; h=vals[2+dim:2+dim+dim*dim]; fisher=vals[2+dim+dim*dim:2+dim+2*dim*dim]; opg=vals[2+dim+2*dim*dim:]
    if f not in compiled:
        dossier=yaml.safe_load((ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{f}/README.md').read_text().split('---')[1])
        syms=[sp.Symbol(v,positive=True) for v in names]; sx=sp.Symbol('x',positive=True)
        # lambda is a Python keyword; map its token before SymPy parsing.
        density=dossier['log_density'].replace('lambda','poisson_lambda')
        symbols=dict(zip(names,syms),x=sx)
        if f=='pois': symbols['poisson_lambda']=syms[0]
        expr=sp.sympify(density,locals=symbols)
        compiled[f]=(sp.lambdify([sx,*syms],expr,'mpmath'),
                     [sp.lambdify([sx,*syms],sp.diff(expr,v),'mpmath') for v in syms],
                     [[sp.lambdify([sx,*syms],-sp.diff(expr,v,w),'mpmath') for w in syms] for v in syms])
    ld,gs,hs=compiled[f]; args=[mp.mpf(x),*map(mp.mpf,p)]
    for j,a in enumerate(free):
        compare('score',s[j],gs[a](*args),2e-13)
        for k,b in enumerate(free):
            idx=j*dim+k
            compare('hessian_symbolic',h[idx],hs[a][b](*args),2e-13)
            compare('opg',opg[idx],gs[a](*args)*gs[b](*args),3e-13)
            # Central differences of the README log density, independent of
            # symbolic Hessians and C++ expressions. High precision removes
            # cancellation; O(step^2) truncation remains.
            da=mp.mpf('0.00001')*max(1,abs(args[a+1])); db=mp.mpf('0.00001')*max(1,abs(args[b+1]))
            def shifted(u,v=0):
                q=args.copy(); q[a+1]+=u; q[b+1]+=v; return ld(*q)
            central=-(shifted(da)-2*ld(*args)+shifted(-da))/da**2 if a==b else -(shifted(da,db)-shifted(da,-db)-shifted(-da,db)+shifted(-da,-db))/(4*da*db)
            compare('hessian_central',h[idx],central,2e-8)
    if mask != (1<<n)-1: continue
    # All 8 points/family: direct integration of scores squared, including
    # nonzero nuisance cross terms. No simulation approximation.
    for a in range(n):
        for b in range(n):
            def integrand(t):
                q=[t,*args[1:]]
                return mp.exp(ld(*q))*gs[a](*q)*gs[b](*q)
            if f=='pois': expected=mp.fsum(integrand(k) for k in range(180))
            elif f=='norm': expected=mp.quad(integrand,[-mp.inf,p[0],mp.inf])
            elif f=='gamma': expected=mp.quad(integrand,[0,1,mp.inf])
            else: expected=mp.quad(integrand,[0,mp.mpf('.5'),1])
            compare('fisher_expectation',fisher[a*n+b],expected,1e-12)
    fisher_seen.add(f)
    if f=='gamma':
        # eta=(shape,rate), theta=(shape,1/rate). Scores and Fisher use
        # Jacobian congruence. Observed information needs -sum s_i theta_i''.
        a,t=p; rate=mp.mpf(1)/t; jac=[mp.mpf(1),-1/rate**2]
        rate_ld=lambda aa,rr: aa*mp.log(rr)-mp.loggamma(aa)+(aa-1)*mp.log(x)-rr*x
        for j in range(2):
            direct=mp.diff(lambda z: rate_ld(z,rate) if j==0 else rate_ld(a,z), a if j==0 else rate)
            compare('transform',s[j]*jac[j],direct,2e-13)
            for k in range(2):
                direct_h=-mp.diff(rate_ld,(mp.mpf(a),rate),(int(j==0)+int(k==0),int(j==1)+int(k==1)))
                pulled=h[j*2+k]*jac[j]*jac[k]-(s[1]*2/rate**3 if j==k==1 else 0)
                compare('transform',pulled,direct_h,2e-13)
                expected_f=mp.polygamma(1,a) if j==k==0 else mp.mpf(a)/rate**2 if j==k==1 else -1/rate
                compare('transform',fisher[j*2+k]*jac[j]*jac[k],expected_f,2e-13)
assert fisher_seen==set(families)
result={'seed':SEED,'points':len(points),'full_points_per_family':8,'mpmath_dps':mp.mp.dps,'max_scaled_errors':maxerr,
        'python':sys.version,'platform':platform.platform(),'mpmath':mp.__version__,'sympy':sp.__version__,
        'compiler':subprocess.check_output([os.environ.get('CXX','c++'),'--version'],text=True).splitlines()[0],
        'source_sha256':{str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [ROOT/'univariateML-cpp/include/cppunivariate/core/inference.hpp', *[ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{f}/score.hpp' for f in families]]},
        'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()}
(OUT/'results.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
