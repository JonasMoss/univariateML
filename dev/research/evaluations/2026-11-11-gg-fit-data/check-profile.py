"""Compare local signed-profile roots with independent 80-digit score roots.

Compile all-probes.cpp into build/scratch/gg-fit-probes before running.
"""
from pathlib import Path
import subprocess
import os
import mpmath as mp

mp.mp.dps=80
root=Path(__file__).resolve().parents[4]
perturbations=[0.1,0.01,1e-4,1e-6,1e-8,1e-10]
proc=subprocess.run([str(root/'build/scratch/gg-fit-probes')],
                    input=''.join(f'{v:.17g}\n' for v in perturbations),
                    text=True,capture_output=True,check=True,env={**os.environ, "GG_PROBE_MODE": "profile"})
rows=proc.stdout.splitlines()
assert len(rows)==len(perturbations)
for perturbation,row in zip(perturbations,rows):
    m,s,q,ok=row.split()
    assert ok=='1'
    # Match the double rounding of -(2+perturbation) in the C++ input.
    y=list(map(mp.mpf,[2.,1.,0.,-1.,-(2.+perturbation)]))
    def scores(mu,sigma,shape):
        w=[(v-mu)/sigma for v in y]
        mu_score=sum(mp.expm1(shape*z)/shape for z in w)/len(w)
        sigma_score=sum(z*mp.expm1(shape*z)/shape for z in w)/len(w)-1
        a=1/shape**2
        remainder=(2/shape)*(a*(mp.log(a)-mp.digamma(a))-mp.mpf('.5'))
        penalty=sum((shape*z*mp.expm1(shape*z)-2*(mp.expm1(shape*z)-shape*z))/shape**3 for z in w)/len(w)
        return mu_score,sigma_score,-remainder-penalty
    expected=mp.findroot(scores,(mp.mpf(m),mp.mpf(s),mp.mpf(q)),tol=mp.mpf('1e-65'))
    errors=[abs(float(m)-float(expected[0])),abs(float(s)/float(expected[1])-1),abs(float(q)-float(expected[2]))]
    print(f'{perturbation:.3g}: max coefficient error {max(errors):.3g}')
    assert max(errors)<2e-10
