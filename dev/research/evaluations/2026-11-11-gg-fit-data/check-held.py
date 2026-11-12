"""Compare fixed-Q conditional fits with independent 80-digit roots.

Compile all-probes.cpp into build/scratch/gg-fit-probes before running.
"""
from pathlib import Path
import subprocess
import os
import mpmath as mp

mp.mp.dps = 80
root = Path(__file__).resolve().parents[4]
y = [mp.mpf(float(v)) for v in (-1.4, -0.3, 0.2, 0.6, 1.1, 1.8)]
fixed_mu, fixed_sigma = mp.mpf(float(0.2)), mp.mpf(float(1.3))
mean = sum(y)/len(y)
qs = [-2., -.2, -1e-8, 0., 1e-8, .2, 2.]
proc = subprocess.run([str(root/'build/scratch/gg-fit-probes')],
                      input=''.join(f'{q:.17g}\n' for q in qs), text=True,
                      capture_output=True, check=True, env={**os.environ, "GG_PROBE_MODE": "held"})
rows = proc.stdout.splitlines()
assert len(rows) == len(qs)

def location(sigma, q):
    if q == 0:
        return mean
    k = mp.log(sum(mp.exp(q*(v-mean)/sigma) for v in y)/len(y))
    return mean + sigma*k/q

def scale_score(log_sigma, mu, q):
    sigma = mp.exp(log_sigma)
    w = [(v-mu)/sigma for v in y]
    if q == 0:
        return sum(z*z for z in w)/len(w)-1
    return sum(z*mp.expm1(q*z)/q for z in w)/len(w)-1

def solve_scale(mu, q):
    return mp.exp(mp.findroot(lambda z: scale_score(z,mu,q), (-10,10),
                              solver='bisect',tol=mp.mpf('1e-65'),maxsteps=300))

for q_float, row in zip(qs, rows):
    q = mp.mpf(q_float)
    l, ls, s, ss, jm, js, status = row.split()
    assert (ls, ss, status) == ('0', '0', '0')
    expected_l = location(fixed_sigma, q)
    expected_s = solve_scale(fixed_mu, q)
    root_log_sigma = mp.findroot(lambda z: scale_score(z,location(mp.exp(z),q),q),
                                (-10,10),solver='bisect',tol=mp.mpf('1e-65'),maxsteps=300)
    expected_js = mp.exp(root_log_sigma)
    expected_jm = location(expected_js,q)
    errors = [abs(float(l)-float(expected_l)), abs(float(s)/float(expected_s)-1),
              abs(float(jm)-float(expected_jm)), abs(float(js)/float(expected_js)-1)]
    print(f'{q_float: .3g}: max coefficient error {max(errors):.3g}')
    assert max(errors) < 3e-10
