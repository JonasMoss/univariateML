"""Compare the signed shape score with 80-digit analytic derivatives.

Compile all-probes.cpp into build/scratch/gg-fit-probes before running.
"""
from pathlib import Path
import subprocess
import os
import mpmath as mp

mp.mp.dps = 80
root = Path(__file__).resolve().parents[4]
y = [mp.mpf(float(v)) for v in (-1.4, -0.3, 0.2, 0.6, 1.1, 1.8)]
mu, sigma = mp.mpf(float(0.2)), mp.mpf(float(1.3))
qs = [0., -1e-12, 1e-12, -1e-8, 1e-8, -1e-4, 1e-4,
      -0.099999, 0.099999, -0.100001, 0.100001,
      -0.249999, 0.249999, -0.25, 0.25, -0.250001, 0.250001,
      -0.4, 0.4, -2., 2.]
p = subprocess.run([str(root / 'build/scratch/gg-fit-probes')],
                   input=''.join(f'{q:.17g}\n' for q in qs), text=True,
                   capture_output=True, check=True, env={**os.environ, "GG_PROBE_MODE": "score"})
rows = p.stdout.splitlines()
assert len(rows) == len(qs)
worst = 0.
for q, row in zip(qs, rows):
    value, valid = row.split()
    assert valid == '1'
    qmp = mp.mpf(q)
    w = [(v-mu)/sigma for v in y]
    if q == 0:
        reference = -sum(z**3 for z in w)/(6*len(w))
    else:
        a = 1/qmp**2
        remainder_deriv = (2/qmp)*(a*(mp.log(a)-mp.digamma(a))-mp.mpf('0.5'))
        penalties = [(qmp*z*mp.expm1(qmp*z)-2*(mp.expm1(qmp*z)-qmp*z))/qmp**3 for z in w]
        reference = -remainder_deriv-sum(penalties)/len(w)
    error = abs(float(value)-float(reference))
    worst = max(worst, error)
    print(f'{q: .3g}  absolute error {error:.3g}')
    assert error < 2e-11
print(f'worst absolute error: {worst:.3g}')
