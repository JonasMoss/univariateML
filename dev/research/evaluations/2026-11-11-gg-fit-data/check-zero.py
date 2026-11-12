"""Check the symmetric Q=0 certificate and independent profile values.

Compile all-probes.cpp into build/scratch/gg-fit-probes before running.
The finite grid comparison is numerical evidence, not the global certificate.
"""
from pathlib import Path
import os
import subprocess
import mpmath as mp
mp.mp.dps=70
root=Path(__file__).resolve().parents[4]
p=subprocess.run([str(root/'build/scratch/gg-fit-probes')],text=True,capture_output=True,
                 check=True,env={**os.environ,'GG_PROBE_MODE':'zero'})
row=p.stdout.split()
assert row[0]=='1',row
print('zero certificate and evaluations:',row)
y=[mp.mpf(k)*mp.log(2) for k in range(-5,6)]
sd=mp.sqrt(sum(v*v for v in y)/len(y))
u=[v/sd for v in y]
for j in range(91):
    t=mp.power(10,mp.mpf(-6)+mp.mpf(j)/10)
    K=mp.log(sum(mp.exp(t*v) for v in u)/len(u))
    initial=mp.log(1/(2*K))
    la=mp.findroot(lambda p:p-mp.digamma(mp.exp(p))-K,(initial,initial+mp.mpf('.01')))
    a=mp.exp(la)
    R=mp.loggamma(a)-(a-mp.mpf('.5'))*mp.log(a)+a-mp.log(2*mp.pi)/2
    delta=mp.log(t)+mp.log(a)/2-R-a*K+mp.mpf('.5')
    assert delta<0
print('91 positive profile points below the attained zero member; symmetry covers the negative side')
