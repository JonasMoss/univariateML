#!/usr/bin/env python3
"""Tukey lambda, two observations, all coefficients free: where is the supremum?

Two observations x1 < x2 at quantile levels u1 < u2 of a member with shape
lambda in the fitting branch [-1, 1] fix sigma = d/(Q(u2) - Q(u1)) and mu,
where d = x2 - x1, Q(u) = (u^lambda - (1-u)^lambda)/lambda (log(u/(1-u)) at
lambda = 0) and Q'(u) = u^(lambda-1) + (1-u)^(lambda-1). The likelihood is

    L = 1/(sigma^2 Q'(u1) Q'(u2)) = h(lambda, u1, u2)/d^2,
    h = (Q(u2) - Q(u1))^2 / (Q'(u1) Q'(u2)),

so the two-point outcome is the same for every sample (equivariance). At
lambda = 1, Q' = 2 and h = (u2 - u1)^2 <= 1, with equality only at u1 = 0,
u2 = 1: the uniform on [x1, x2] (mu = (x1 + x2)/2, sigma = d/2), an attained
member of the closed branch. The claim to check is H(lambda) = sup h < 1 for
every lambda < 1, so that this member is the unique global maximum.

Method (numerical evidence, not a proof): for each lambda on a grid of 401
points in [-1, 1) plus lambda = 1 - 10^-j (j = 1..8), maximize log h over
(v1, v2) = logit(u1, u2), v1 < v2, by Nelder-Mead from 64 starts on
[-30, 30]^2 (double precision), and report H(lambda), 1 - H and whether the
maximizer is symmetric (v1 = -v2). A symmetric argument (memo, sketch) covers
v1 = -v2: there h = (2 z g(z))^2 <= 1 because the mass of [-z, z] is at least
2 z g(z) for the unimodal standardized density g.
Usage: python3 tlambda_two_point.py > tlambda_two_point.txt
"""
import json, math
import numpy as np
from scipy.optimize import minimize

def log_u(v): return -np.logaddexp(0, -v)          # log u, u = logistic(v)

def Q(lam, v):
    if lam == 0: return v
    return (np.exp(lam*log_u(v)) - np.exp(lam*log_u(-v)))/lam

def log_dQ(lam, v):
    return np.logaddexp((lam-1)*log_u(v), (lam-1)*log_u(-v))

def log_h(lam, w):
    v1, v2 = w
    if v2 <= v1: return -np.inf
    gap = Q(lam, v2) - Q(lam, v1)
    if not gap > 0: return -np.inf
    return 2*math.log(gap) - log_dQ(lam, v1) - log_dQ(lam, v2)

def H(lam):
    best = (-np.inf, None)
    starts = [(a, b) for a in np.linspace(-30, 28, 8) for b in np.linspace(-28, 30, 8) if b > a]
    for s in starts:
        r = minimize(lambda w: -log_h(lam, w), s, method='Nelder-Mead',
                     options=dict(xatol=1e-10, fatol=1e-14, maxiter=4000, maxfev=8000))
        if -r.fun > best[0]: best = (-r.fun, r.x)
    return best

grid = [-1+k/200 for k in range(400)] + [1-10.0**-j for j in range(1, 9)]
rows = []
for lam in grid:
    value, w = H(lam)
    rows.append(dict(lam=lam, H=math.exp(value), margin=1-math.exp(value), v1=w[0], v2=w[1], symmetric=abs(w[0]+w[1]) < 1e-4*max(1, abs(w[1]))))
worst = max(rows, key=lambda r: r['H'])
print('lambda grid: %d points in [-1, 1); max H = %.15g at lambda = %.10g (1 - H = %.3g)' % (len(rows), worst['H'], worst['lam'], worst['margin']))
print('asymmetric maximizers:', [r['lam'] for r in rows if not r['symmetric']])
print('H increasing in lambda on the grid:', all(a['H'] <= b['H'] for a, b in zip(rows, rows[1:])))
for r in rows[::25] + rows[400:]:
    print('lambda %+.10f  H %.15f  1-H %.3e  v1 %+.6f  v2 %+.6f  symmetric %s' % (r['lam'], r['H'], r['margin'], r['v1'], r['v2'], r['symmetric']))
print('lambda = 1: h = (u2 - u1)^2, maximum 1 at u1 = 0, u2 = 1 (exact)')
