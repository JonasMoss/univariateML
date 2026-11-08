"""Independent 70-digit analytic-score root; does not edit the golden.
Run: /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-08-bhatt-cert-data/golden-score.py
"""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps = 70
root = Path(__file__).resolve().parents[4]
g = json.loads((root / 'univariateML-cpp/include/cppunivariate/distributions/bhatt/golden.json').read_text())
x = [mp.mpf(v) for v in g['data']]
p = [mp.mpf(g['estimates'][k]) for k in ('mu', 'sigma', 'a')]
phi = lambda z: mp.exp(-z*z/2)/mp.sqrt(2*mp.pi)
Phi = lambda z: mp.erfc(-z/mp.sqrt(2))/2

def scores(m, s, a):
    sm = ss = sa = mp.mpf(0)
    for v in x:
        lo = (v-m-a)/s
        hi = (v-m+a)/s
        d = Phi(hi)-Phi(lo)
        A = phi(lo)/d
        B = phi(hi)/d
        sm += (A-B)/s
        ss += (lo*A-hi*B)/s
        sa += (A+B)/s-1/a
    return sm, ss, sa

r = mp.findroot(scores, tuple(p), tol=mp.mpf('1e-60'))
print(json.dumps({'digits': 70, 'golden_scores': [mp.nstr(z, 60) for z in scores(*p)],
                  'stationary_coefficients': [mp.nstr(z, 60) for z in r],
                  'relative_changes': [mp.nstr((a-b)/b, 60) for a, b in zip(r, p)],
                  'root_scores': [mp.nstr(z, 10) for z in scores(*r)]}, indent=2))
