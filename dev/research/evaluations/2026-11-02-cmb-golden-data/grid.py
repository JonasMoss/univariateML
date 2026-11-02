"""Numerical fixed-size profiles for the unchanged CMB golden sample.

Run from the repository root with python3; requires the existing scipy/numpy.
These local optimizations are a numerical cross-check, not a tail certificate.
"""
import json
import time
import numpy as np
from scipy.optimize import minimize_scalar, root
from scipy.special import gammaln, logsumexp

with open('univariateML-cpp/include/cppunivariate/distributions/cmb/golden.json') as f:
    golden = json.load(f)
x = np.asarray(golden['data'], dtype=int)
n = len(x)
sx = x.sum()
held_eta = np.log(golden['estimates']['prob'] / (1 - golden['estimates']['prob']))
for fixed in (False, True):
    start = time.monotonic()
    best = (-np.inf, None, None)
    failed = []
    for size in range(int(x.max()), 501):
        k = np.arange(size + 1)
        h = gammaln(size + 1) - gammaln(k + 1) - gammaln(size - k + 1)
        target_h = h[x].sum()
        def objective(eta, nu):
            z = eta * k + nu * h
            return sx * eta + target_h * nu - n * logsumexp(z)
        if fixed:
            fit = minimize_scalar(lambda nu: -objective(held_eta, nu), bracket=(-1, 1), method='brent', options={'xtol': 1e-11})
            theta = np.array([held_eta, fit.x])
            success = fit.success
        else:
            def score(theta):
                weights = np.exp(theta[0] * k + theta[1] * h - logsumexp(theta[0] * k + theta[1] * h))
                return np.array([sx / n - weights @ k, target_h / n - weights @ h])
            fit = root(score, [-1, 1], tol=1e-10)
            theta = fit.x
            success = np.max(np.abs(score(theta))) < 1e-8
        if not success:
            failed.append(size)
        ll = objective(*theta)
        if ll > best[0]:
            best = (ll, size, theta)
    print('fixed', fixed, 'best', best, 'failed', failed, 'seconds', time.monotonic() - start, flush=True)
