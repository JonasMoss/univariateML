"""Independent 70-digit original-shape scores, frozen golden and saturated tables.
Run through /tmp/umvl-cpu/run; --binary may name a separately compiled checker.
"""
import argparse
from collections import Counter
from fractions import Fraction
import json
import math
import os
from pathlib import Path
import random
import subprocess
import mpmath as mp

mp.mp.dps = 70
root = Path.cwd()
folder = Path(__file__).resolve().parent
parser = argparse.ArgumentParser()
parser.add_argument('--binary')
args = parser.parse_args()
if args.binary:
    binary = args.binary
else:
    boost = subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
    binary = str(root/'build/scratch/bbinom-native')
    subprocess.run(['c++','-O2','-std=c++17',f'-I{boost}','-IunivariateML-cpp/include',
                    str(folder/'native-check.cpp'),'-o',binary],check=True)

golden = json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/bbinom/golden.json').read_text())
cases = []
for name,N,x in [('golden',10,golden['data']),('ordinary',10,[0,1,1,2,2,3,4,5,7,10]),
                 ('near-face',3,[v for v,f in enumerate([12501,37499,37499,12501]) for _ in range(f)])]:
    for a,b in [(0,0),(2,0),(0,3)]:
        cases.append(dict(name=name,N=N,x=x,a=a,b=b))
for N,a,b in [(3,2,3),(6,1,1),(10,2,3),(4,1,5),(8,5,1)]:
    def rising(t,k):
        return math.prod(range(t,t+k))
    probs=[Fraction(math.comb(N,k)*rising(a,k)*rising(b,N-k),rising(a+b,N)) for k in range(N+1)]
    denom=math.lcm(*(p.denominator for p in probs))
    x=[v for v,p in enumerate(probs) for _ in range(int(p*denom))]
    for held_a,held_b in [(0,0),(a,0),(0,b)]:
        cases.append(dict(name=f'saturated-{N}-{a}-{b}',N=N,x=x,a=held_a,b=held_b,exact=[a,b]))
rng=random.Random(11650)
for i in range(12):
    N=rng.randrange(3,15)
    x=[0,N]+[rng.randrange(N+1) for _ in range(28)]
    for a,b in [(0,0),(.4,0),(0,8)]:
        cases.append(dict(name=f'seed-{i}',N=N,x=x,a=a,b=b))
request=''.join(f"{c['N']} {c['a']} {c['b']} {len(c['x'])} "+' '.join(map(str,c['x']))+'\n' for c in cases)
lines=subprocess.check_output([binary],input=request,text=True).splitlines()
assert len(lines)==len(cases)
results=[]
for c,line in zip(cases,lines):
    status,aa,bb,work=line.split(); status=int(status);aa=float(aa);bb=float(bb)
    assert status==0,(c['name'],c['a'],c['b'],status)
    result={k:v for k,v in c.items() if k!='x'}
    result.update(counts=dict(Counter(c['x'])),status=status,alpha=aa,beta=bb,cells=int(work))
    if status==0:
        N=c['N'];counts=Counter(c['x']);h=len(c['x'])
        # Independent analytic derivatives of log beta ratios in alpha,beta.
        def score_a(a,b):
            return sum(n*(mp.digamma(k+a)-mp.digamma(a)) for k,n in counts.items())-h*(mp.digamma(N+a+b)-mp.digamma(a+b))
        def score_b(a,b):
            return sum(n*(mp.digamma(N-k+b)-mp.digamma(b)) for k,n in counts.items())-h*(mp.digamma(N+a+b)-mp.digamma(a+b))
        if c['a']:
            refa=mp.mpf(c['a']);refb=mp.findroot(lambda b:score_b(refa,b),(mp.mpf(bb)*mp.mpf('.999'),mp.mpf(bb)*mp.mpf('1.001')))
        elif c['b']:
            refb=mp.mpf(c['b']);refa=mp.findroot(lambda a:score_a(a,refb),(mp.mpf(aa)*mp.mpf('.999'),mp.mpf(aa)*mp.mpf('1.001')))
        else:
            refa,refb=mp.findroot((score_a,score_b),(mp.mpf(aa),mp.mpf(bb)))
        errors=[float(abs(mp.mpf(aa)/refa-1)),float(abs(mp.mpf(bb)/refb-1))]
        assert max(errors)<=1e-10,(c['name'],errors)
        if 'exact' in c:
            assert max(abs(refa/c['exact'][0]-1),abs(refb/c['exact'][1]-1))<mp.mpf('1e-50')
        if c['name']=='golden' and not c['a'] and not c['b']:
            assert max(abs(aa/golden['estimates']['alpha']-1),abs(bb/golden['estimates']['beta']-1))<1e-8
        result.update(reference=[str(refa),str(refb)],relative_error=errors)
    results.append(result)
print(json.dumps(dict(precision=70,case_count=len(cases),results=results),indent=2))
