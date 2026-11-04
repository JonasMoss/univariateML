#!/usr/bin/env python3
"""Independent exact-rational Sturm check and old/new timing reproduction.
Run from repository root under /tmp/umvl-cpu/run; no third-party Python modules.
All generated files stay in build/scratch/cauchy-check. Baseline is read via git show.
"""
import fractions
import json
import math
import pathlib
import random
import subprocess

ROOT = pathlib.Path(__file__).resolve().parents[4]
OUT = ROOT / 'build/scratch/cauchy-check'
OUT.mkdir(parents=True, exist_ok=True)
F = fractions.Fraction


def trim(p):
    while len(p) > 1 and p[-1] == 0:
        p.pop()
    return p


def add(a, b):
    c = [F(0)] * max(len(a), len(b))
    for j, v in enumerate(a): c[j] += v
    for j, v in enumerate(b): c[j] += v
    return trim(c)


def mul(a, b):
    c = [F(0)] * (len(a) + len(b) - 1)
    for i, v in enumerate(a):
        for j, w in enumerate(b): c[i+j] += v*w
    return trim(c)


def remainder(a, b):
    a = list(a)
    while len(a) >= len(b) and any(a):
        factor, shift = a[-1]/b[-1], len(a)-len(b)
        for j, v in enumerate(b): a[j+shift] -= factor*v
        trim(a)
    return a


def evaluate(p, x):
    total = F(0)
    for v in reversed(p): total = total*x+v
    return total


def stationary_roots(x):
    # Exact numerator of sum (x_i-t)/(1+(x_i-t)^2), without factor 2.
    polynomial = [F(0)]
    for i, v in enumerate(x):
        term = [F(v), F(-1)]
        for j, w in enumerate(x):
            if i != j: term = mul(term, [F(1+w*w), F(-2*w), F(1)])
        polynomial = add(polynomial, term)
    chain = [polynomial, [j*polynomial[j] for j in range(1,len(polynomial))]]
    while True:
        r = [-v for v in remainder(chain[-2],chain[-1])]
        if not any(r): break
        # Positive scaling controls coefficient growth, preserves all signs.
        r = [v/abs(r[-1]) for v in r]
        chain.append(r)

    def variations(t):
        signs = [1 if v > 0 else -1 for p in chain if (v := evaluate(p,t)) != 0]
        return sum(a != b for a,b in zip(signs,signs[1:]))

    pending = [(F(min(x)-1),F(max(x)+1))]
    roots = []
    while pending:
        a,b = pending.pop()
        count = variations(a)-variations(b)
        if not count: continue
        if count == 1 and b-a < F(1,10**14):
            roots.append(float((a+b)/2)); continue
        # Avoid roots exactly at a partition endpoint (including symmetry).
        for denominator in range(2,100):
            t = a+(b-a)/denominator
            if evaluate(polynomial,t) != 0: break
        else: raise AssertionError('no nonroot split')
        pending.extend([(a,t),(t,b)])
    return sorted(roots)


HEADER = ROOT/'univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp'
old = subprocess.check_output(['git','show','54b0d06a:univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp'],cwd=ROOT,text=True)
old = old.replace('"../../core/', '"cppunivariate/core/')
(OUT/'old.hpp').write_text(old)
driver = r'''
#include HEADER
#include <chrono>
#include <iomanip>
#include <iostream>
int main() {
  int n; double scale;
  while (std::cin >> n >> scale) {
    std::vector<double> x(n); for(double& v:x)std::cin>>v;
    const auto start=std::chrono::steady_clock::now();
    const auto r=cppunivariate::fit_cauchy({x.data(),x.size()},1e-8,100,NAN,scale);
    std::cout<<std::setprecision(17)<<(int)r.status<<" "<<r.mu<<" "<<r.logLik<<" "<<r.iterations<<" "
      <<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<std::endl;
  }
}
'''
(OUT/'driver.cpp').write_text(driver)
for name, header, extra in [('new',HEADER,[]),('double',HEADER,['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS']),('old',OUT/'old.hpp',[])]:
    subprocess.run(['g++','-O2','-std=c++17',*extra,'-I'+str(ROOT/'univariateML-cpp/include'),'-DHEADER="'+str(header)+'"',str(OUT/'driver.cpp'),'-o',str(OUT/name)],check=True)


def fit(name, x, scale=1):
    request = f'{len(x)} {scale} '+' '.join(map(repr,x))+'\n'
    line = subprocess.check_output([str(OUT/name)],input=request,text=True).split()
    return dict(status=int(line[0]),mu=float(line[1]),ll=float(line[2]),boxes=int(line[3]),seconds=float(line[4]))


rng = random.Random(116)
samples = [[-2,2],[-3,-2,2,3],[-2,-2,2],[-20,-19,0,1,2,20],[-10,0,0,0,3,15]]
samples += [rng.sample(range(-20,21),rng.randrange(3,8)) for _ in range(32)]
checks = []
for x in samples:
    roots = stationary_roots(x)
    values = [-len(x)*math.log(math.pi)-math.fsum(math.log1p((v-t)**2) for v in x) for t in roots]
    best = max(values)
    winners = [t for t,v in zip(roots,values) if abs(v-best)<1e-10]
    for name in ['new','double']:
        r = fit(name,x)
        if len(winners)>1:
            assert r['status']==4,(x,winners,r)
        else:
            assert r['status']==0,(x,winners,r)
            assert abs(r['mu']-winners[0])<2e-8,(x,winners,r)
            assert abs(r['ll']-best)<1e-10,(x,winners,r)
    checks.append(dict(x=x,roots=len(roots),global_maxima=len(winners),maximizers=winners,logLik=best))
print(f'Exact rational Sturm oracle: {len(samples)} samples x two precision modes passed',flush=True)

benchmarks = []
for n in [100,1000,100000]:
    x = [math.tan(math.pi*((j+.37)/n-.5)) for j in range(n)]
    for name in (['old','new','double'] if n<=1000 else ['new','double']):
        r=fit(name,x)
        assert r['status']==0,(n,name,r)
        benchmarks.append(dict(workload='Cauchy quantiles',n=n,implementation=name,**r))
        print(benchmarks[-1],flush=True)
# Large n does not mean unimodal: imbalanced clusters and tied separated modes.
for label, x, expected in [
    ('imbalanced clusters',[-10.0]*40000+[10.0]*60000,0),
    ('tied clusters',[-10.0]*50000+[10.0]*50000,4),
    ('outliers',[math.tan(math.pi*((j+.37)/99999-.5)) for j in range(99999)]+[1e12],0),
]:
    for name in (['old','new','double'] if label != 'outliers' else ['new','double']):
        r=fit(name,x)
        assert r['status']==(0 if name == 'old' else expected),(label,name,r)
        benchmarks.append(dict(workload=label,n=len(x),implementation=name,**r))
        print(benchmarks[-1],flush=True)
(OUT/'results.json').write_text(json.dumps(dict(checks=checks,benchmarks=benchmarks),indent=2)+'\n')
