"""Critical GED likelihood increments and high-precision distance checks."""
import json
import hashlib
import math
from pathlib import Path

import mpmath as mp
import numpy as np
from scipy import integrate, special

HERE = Path(__file__).resolve().parent
mp.mp.dps = 50
reference = []
base = json.loads((HERE/'results.json').read_text())
for family, shape, h in [('gpd', 4/3, .00003), ('gpd', 2, .00003),
                          ('gev', 2, .00003), ('ged', .5, .00003)]:
    p, delta = mp.mpf(str(shape)), mp.mpf(str(h))
    if family == 'gpd':
        f = lambda d: (mp.sqrt(p*d**(p-1))-mp.sqrt(p*(d+delta)**(p-1)/(1+delta)**p))**2
        value = (delta/(1+delta))**p+mp.quad(f, [0, delta, 10*delta, .1, 1])
    elif family == 'gev':
        f = lambda d: (mp.sqrt(p*d**(p-1)*mp.exp(-d**p))-
                       mp.sqrt(p*(d+delta)**(p-1)*mp.exp(-(d+delta)**p)))**2
        value = -mp.expm1(-delta**p)+mp.quad(f, [0, delta, 10*delta, .1, 1, mp.inf])
    else:
        C = p/(2*mp.gamma(1/p))
        f = lambda x: C*(mp.exp(-abs(x)**p/2)-mp.exp(-abs(x-delta)**p/2))**2
        value = mp.quad(f, [-mp.inf, -1, -delta, 0, delta, 2*delta, 1, mp.inf])
    prior = next(r['H2'] for r in base['distances'] if r['family'] == family and r['shape'] == shape and r['h'] == h)
    relative = abs(float(value)/prior-1)
    assert relative < 2e-6, (family, shape, value, prior)
    reference.append(dict(family=family, shape=shape, h=h, mpmath_H2=str(value),
                          scipy_H2=prior, relative_difference=relative))

rng = np.random.default_rng(20261105)
rows = []
reps = 2000
for n in [10000, 100000]:
    h = 1/math.sqrt(n*math.log(n))
    llr, opg = [], []
    for start in range(0, reps, 20):
        # nu=1/2, a=1: sqrt(abs(X)) ~ Gamma(2,1).
        r = rng.gamma(2, size=(20, n))
        x = r*r*rng.choice([-1, 1], size=(20, n))
        llr.extend(np.sum(r-np.sqrt(abs(x-h)), axis=1))
        opg.extend(np.sum(.25/(r*r), axis=1)/(n*math.log(n)))
    # Independent finite-h Hellinger integral, using sqrt-density differences.
    def f(x):
        l0, l1 = -math.sqrt(abs(x)), -math.sqrt(abs(x-h))
        return .25*math.exp(max(l0, l1))*math.expm1(-abs(l0-l1)/2)**2
    pts = [-np.inf, -1, -h, 0, h, 2*h, 1, np.inf]
    H2 = sum(integrate.quad(f, a, b, epsabs=1e-17, epsrel=1e-8)[0] for a, b in zip(pts[:-1], pts[1:]))
    mean, var = float(np.mean(llr)), float(np.var(llr, ddof=1))
    assert abs(var/(4*n*H2)-1) < .2
    assert abs(mean+2*n*H2) < 5*math.sqrt(var/reps)+.002
    rows.append(dict(n=n, reps=reps, h=h, mean_log_LR=mean, variance_log_LR=var,
                     approximate_finite_h_drift=-2*n*H2, approximate_finite_h_variance=4*n*H2,
                     asymptotic_LAN_information=.0625,
                     raw_OPG_over_n_log_n_quantiles=np.quantile(opg, [.1, .5, .9]).tolist(),
                     raw_OPG_probability_limit=.125,
                     hypothesis='mu=0 versus mu=h; nu=1/2 and scale a=1 known'))
out = dict(seed=20261105, mpmath_version=mp.__version__, dps=mp.mp.dps,
           revision=base['revision'],
           script_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
           main_results_sha256=hashlib.sha256((HERE/'results.json').read_bytes()).hexdigest(),
           distance_comparisons=reference, critical_GED=rows, assertions_passed=8)
(HERE/'critical-results.json').write_text(json.dumps(out, indent=2)+'\n')
print(json.dumps(out, indent=2))
