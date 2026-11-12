"""Check the strict face outcomes and independent 70-digit profile values.

Compile all-probes.cpp into build/scratch/gg-fit-probes before running.
The grid comparison is numerical evidence, not the global certificate.
"""
from pathlib import Path
import math
import os
import subprocess
import mpmath as mp
mp.mp.dps=70
root=Path(__file__).resolve().parents[4]
p=subprocess.run([str(root/'build/scratch/gg-fit-probes')],text=True,capture_output=True,
                 check=True,env={**os.environ,'GG_PROBE_MODE':'face'})
rows=p.stdout.splitlines()
assert len(rows)==3
outcomes=[int(row.split()[0]) for row in rows]
assert outcomes==[1,2,0], outcomes
print('face outcomes and certificate evaluations:',rows)
y=list(map(mp.mpf,[0.,math.log(2.),math.log(3.)]))
mean=sum(y)/len(y)
sd=mp.sqrt(sum((v-mean)**2 for v in y)/len(y))
u=[(v-mean)/sd for v in y]
target=mp.log(2*mp.pi)/2-mp.mpf('.5')-mp.log(max(u))
for sign in [1,-1]:
    z=[sign*v for v in u]
    highest=-mp.inf
    for j in range(91):
        t=mp.power(10,mp.mpf(-6)+mp.mpf(j)/10)
        K=mp.log(sum(mp.exp(t*v) for v in z)/len(z))
        initial=mp.log(1/(2*K))
        la=mp.findroot(lambda p:p-mp.digamma(mp.exp(p))-K,(initial,initial+mp.mpf('.01')))
        a=mp.exp(la)
        R=mp.loggamma(a)-(a-mp.mpf('.5'))*mp.log(a)+a-mp.log(2*mp.pi)/2
        delta=mp.log(t)+mp.log(a)/2-R-a*K+mp.mpf('.5')
        highest=max(highest,delta)
        assert delta<target
    print(f'side {sign:+d}: smallest grid gap {float(target-highest):.3g}')
