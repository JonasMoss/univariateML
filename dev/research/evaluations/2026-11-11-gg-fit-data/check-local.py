"""Compare the local information check with an independent 80-digit root.

Compile all-probes.cpp into build/scratch/gg-fit-probes before running.
All nonempty free-coordinate subsets are checked at one interior stationary
point, with held coordinates rounded to double before the independent solve.
"""
from pathlib import Path
import subprocess
import os
import mpmath as mp

mp.mp.dps=80
root=Path(__file__).resolve().parents[4]
y=list(map(mp.mpf,[-1.4,-0.3,0.2,0.6,1.1,1.8]))

def scores(mu,sigma,shape):
    w=[(v-mu)/sigma for v in y]
    sm=sum(mp.expm1(shape*z)/shape for z in w)/len(w)
    ss=sum(z*mp.expm1(shape*z)/shape for z in w)/len(w)-1
    a=1/shape**2
    remainder=(2/shape)*(a*(mp.log(a)-mp.digamma(a))-mp.mpf('.5'))
    penalty=sum((shape*z*mp.expm1(shape*z)-2*(mp.expm1(shape*z)-shape*z))/shape**3 for z in w)/len(w)
    return sm,ss,-remainder-penalty

full=mp.findroot(scores,(mp.mpf('.4'),mp.mpf('1'),mp.mpf('.2')),tol=mp.mpf('1e-65'))
rows=[]
references=[]
for mask in range(1,8):
    free=[i for i in range(3) if mask & (1<<i)]
    fixed=[mp.mpf(float(v)) for v in full]
    def residual(*args):
        p=fixed.copy()
        for i,v in zip(free,args): p[i]=v
        s=scores(*p)
        return s[free[0]] if len(free)==1 else tuple(s[i] for i in free)
    r=mp.findroot(residual,tuple(full[i] for i in free),tol=mp.mpf('1e-65'))
    if len(free)==1: r=[r]
    expected=fixed.copy()
    for i,v in zip(free,r): expected[i]=v
    start=[float(v) for v in expected]
    for i in free: start[i] += 1e-5*(1 if i!=1 else start[i])
    rows.append(' '.join(f'{x:.17g}' for x in start)+f' {mask}\n')
    references.append(expected)
proc=subprocess.run([str(root/'build/scratch/gg-fit-probes')],input=''.join(rows),
                    text=True,capture_output=True,check=True,env={**os.environ, "GG_PROBE_MODE": "local"})
output=proc.stdout.splitlines()
assert len(output)==7
for mask,(row,expected) in enumerate(zip(output,references),1):
    mu,sigma,q,ok=row.split()
    assert ok=='1', (mask,row)
    errors=[abs(float(mu)-float(expected[0])),abs(float(sigma)/float(expected[1])-1),abs(float(q)-float(expected[2]))]
    print(f'free mask {mask}: max coefficient error {max(errors):.3g}')
    assert max(errors)<3e-10
