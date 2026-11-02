"""Numerical spot check of candidate sn profile upper bounds; not a proof."""
import numpy as np
from scipy.special import log_ndtr
from scipy.optimize import minimize_scalar

rng = np.random.default_rng(116)
worst = 0.0
for trial in range(200):
    n = 40
    x = rng.normal(size=n)
    x = (x - x.mean()) / x.std()
    a0 = rng.uniform(-5, 5)
    eta0 = np.exp(rng.uniform(-1, 1))
    h0 = rng.uniform(-2, 2)
    z0 = eta0*x-h0
    t0 = a0*z0
    r = np.exp(-t0*t0/2 - np.log(2*np.pi)/2 - log_ndtr(t0))
    c = np.sum(log_ndtr(t0)-r*t0) + n*(np.log(2)-np.log(2*np.pi)/2)
    q, s = np.sum(r*x), np.sum(r)
    def tangent(a):
        d = a*q/n
        eta = (d + np.hypot(d, 2))/2
        h = -a*s/n
        return c+n*np.log(eta)-n*(eta*eta+h*h)/2+a*(eta*q-h*s)
    lo, hi = sorted(rng.uniform(-10,10,2))
    bound = max(tangent(lo), tangent(hi))
    for _ in range(20):
        a = rng.uniform(lo,hi)
        eta = np.exp(rng.uniform(-2,2))
        h = rng.uniform(-3,3)
        z = eta*x-h
        ll = n*np.log(eta)+np.sum(np.log(2)-np.log(2*np.pi)/2-z*z/2+log_ndtr(a*z))
        worst = max(worst, ll-bound)
        assert ll <= bound + 1e-9*n
    A = np.exp(rng.uniform(-1,3))
    def Q(c):
        d = x-c
        return np.sum(d*d*(1+A*A*(d<0)))
    opt = minimize_scalar(Q, bounds=(x.min(), x.max()), method='bounded', options={'xatol': 1e-13})
    # The optimizer is numerical, so allow for its Q overestimate.
    tail = n*(np.log(2)-np.log(2*np.pi)/2 + np.log(n/opt.fun)/2 - .5)
    for _ in range(20):
        a = A*np.exp(rng.uniform(0,3))
        eta = np.exp(rng.uniform(-2,2))
        h = rng.uniform(-3,3)
        z = eta*x-h
        ll = n*np.log(eta)+np.sum(np.log(2)-np.log(2*np.pi)/2-z*z/2+log_ndtr(a*z))
        worst = max(worst, ll-tail)
        assert ll <= tail + 1e-7*n
print(f'200 seeded datasets, 8000 feasible probes; maximum positive excess {worst:g}')
