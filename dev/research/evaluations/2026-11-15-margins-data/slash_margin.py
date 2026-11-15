"""slash, half-mass collapse versus interior: where does the 1e-8 n tie rule select collapse although an MLE exists?

Standard slash density f(z) = (1 - exp(-z^2/2)) / (sqrt(2 pi) z^2), f(0) = 1/(2 sqrt(2 pi)); location m, scale s.
Dossier (slash/README.md, 'exact existence criterion'): put c = 1/sqrt(2 pi); for a value v of multiplicity n/2
    B_v = c^n 2^(-n/2) prod_{x_i != v} |x_i - v|^(-2)           (the collapse log-likelihood is log B_v)
An MLE exists iff some interior profile value L(m, s(m)) >= max_v B_v.  The core (slash/mle.hpp line 406) returns
CollapseLimit (nonexistence, unattained) when  log B_v >= best_interior - 1e-8 n.
Here: samples (0, 0, -1, t), t = 1+eps (v = 0, k = 2 = n/2).  The interior supremum is found by multistart in
(m, log s) with mpmath refinement; the difference D = best_interior - log B is reported against 1e-8 n = 4e-8.
"""
import mpmath as mp, math, sys
import numpy as np
from scipy.optimize import minimize
mp.mp.dps = 40

def logf(z):   # mp
    if z == 0: return -mp.log(2*mp.sqrt(2*mp.pi))
    t = z*z/2
    return mp.log(-mp.expm1(-t))-t-mp.log(2*t)+mp.log(2*t)-mp.log(mp.sqrt(2*mp.pi)*z*z)+t   # = log(1-e^-t) - log(sqrt(2pi) z^2)

def ll(x, m, s):
    return sum(logf((v-m)/s) for v in x)-len(x)*mp.log(s)

def logB(x, v):
    n = len(x); c = 1/mp.sqrt(2*mp.pi)
    return n*mp.log(c)-n*mp.log(2)/2-2*sum(mp.log(abs(mp.mpf(a)-v)) for a in x if a != v)

def ll_np(x, m, ls):
    s = math.exp(ls); out = 0.0
    for v in x:
        z = (v-m)/s; t = z*z/2
        out += (math.log(-math.expm1(-t))-math.log(math.sqrt(2*math.pi)*z*z)) if t > 1e-6 else -math.log(2*math.sqrt(2*math.pi))
    return out-len(x)*ls

def interior_best(x):
    """multistart; returns (best value, m, s) with the near-collapse path m = c s^2 explored by log-scale starts"""
    best = (-1e300, None, None)
    rng = np.random.default_rng(1)
    lo, hi = min(x), max(x)
    starts = [(rng.uniform(lo, hi), rng.uniform(-6, 1)) for _ in range(300)]
    starts += [(c*math.exp(2*ls), ls) for ls in (-8, -6, -4, -3, -2) for c in (-3, -1, -.3, .3, 1, 3)]
    for m0, ls0 in starts:
        r = minimize(lambda p: -ll_np(x, p[0], p[1]), [m0, ls0], method='Nelder-Mead', options=dict(xatol=1e-13, fatol=1e-16, maxiter=4000))
        if -r.fun > best[0]: best = (-r.fun, r.x[0], math.exp(r.x[1]))
    return best

if __name__ == '__main__':
    print('sample (0, 0, -1, t):  slope sum 1/(x_i - 0) = -1 + 1/t')
    print('%10s %12s %14s %14s %12s %10s' % ('t', 'log B_0', 'best interior', 'D = int - B', 'D / (n 1e-8)', 'at (m, s)'))
    n = 4
    for t in ('1.0', '1.0001', '1.001', '1.01', '1.1', '1.5', '2', '3', '0.9', '0.5'):
        x = [0.0, 0.0, -1.0, float(t)]
        b, m, s = interior_best(x)
        Bv = logB([mp.mpf(a) for a in x], mp.mpf(0))
        # refine in mp
        f = lambda mm, ss: ll(x, mm, ss)
        try:
            r = mp.findroot(lambda mm, ls: [mp.diff(lambda a: ll(x, a, mp.e**ls), mm), mp.diff(lambda a: ll(x, mm, mp.e**a), ls)], (mp.mpf(m), mp.log(mp.mpf(s))), tol=1e-25, maxsteps=40)
            val = ll(x, r[0], mp.e**r[1]); where = (mp.nstr(r[0], 6), mp.nstr(mp.e**r[1], 6))
        except Exception as e:
            val = mp.mpf(b); where = (m, s)
        D = val-Bv
        print('%10s %12s %14s %14s %12s   %s' % (t, mp.nstr(Bv, 12), mp.nstr(val, 12), mp.nstr(D, 6), mp.nstr(D/(n*mp.mpf('1e-8')), 5), where))
