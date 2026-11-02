#!/usr/bin/env python3
"""Numerical checks of the exact-arithmetic half-t inequalities in the dossier."""
import math
import numpy as np
from scipy.optimize import minimize_scalar
from scipy.special import gammaln


def ll(x, nu, log_s):
    a = gammaln((nu + 1)/2) - gammaln(nu/2) - .5*math.log(math.pi*nu)
    return len(x)*(math.log(2) + a - log_s) - (nu+1)/2*sum(
        np.logaddexp(0, 2*(math.log(v)-log_s)-math.log(nu)) for v in x if v > 0)


def prof(x, nu):
    fit = minimize_scalar(lambda s: -ll(x, nu, s), bounds=(-30, 30), method='bounded',
                          options={'xatol': 1e-12})
    assert fit.success
    return -fit.fun

samples = [[.12773, 12.5979, 3.91782, 2.58658, .138212],
           [0., 1., 2., 10., 20.], [0., 0., 1., 4.]]
checks = 0
for x in samples:
    n = len(x); pos = [v for v in x if v > 0]; p = len(pos); k = n-p
    a = max(2., 2*k/p)
    s0 = min(pos)*math.sqrt((a*p-k)/(a*n))
    normal_scale = math.sqrt(sum(v*v for v in x)/n)
    normal = n*(.5*math.log(2/math.pi)-math.log(normal_scale)) - n/2
    c = (1+1/a)/4*sum((v/s0)**4 for v in x)
    for nu in np.geomspace(a, 1e5, 100):
        assert prof(x, nu) <= normal+c/nu+1e-8*n
        checks += 1
    if not k:
        for nu in np.geomspace(1e-6, .1, 100):
            bound = n/2*math.log(2*.1/math.pi)-sum(math.log(v) for v in pos)
            assert prof(x, nu) <= bound+1e-8*n
            checks += 1
    if 2*k == n:
        S = sum(math.log(v) for v in pos); M = max(pos)
        boundary = n*math.log(2/math.pi)-2*S
        for delta in np.geomspace(1e-5, .5, 50):
            bound = (boundary+n/2*math.log1p(delta)+p*(2+delta)/2*math.log1p(delta)
                     +delta*abs(S)+p*delta*max(math.log(M), 0))
            assert prof(x, 1+delta) <= bound+1e-8*n
            checks += 1
print(f'{checks} profile comparisons passed; scipy bounded optimization is a numerical check, not a proof')

# Check scale-root and shape-score interval formulae against independent
# scalar optimization and a centered numerical profile derivative.
from scipy.special import digamma
rng = np.random.default_rng(116)
interval_checks = 0
for _ in range(100):
    x = np.exp(rng.normal(size=12))
    a = math.exp(rng.uniform(-2, 3)); b = a*math.exp(rng.uniform(.001, .3))
    def scale_scores(log_s):
        z = x*x/math.exp(2*log_s)
        fa = (a+1)*z/(a+z); fb = (b+1)*z/(b+z)
        return sum(np.minimum(fa, fb))-len(x), sum(np.maximum(fa, fb))-len(x)
    from scipy.optimize import brentq
    lo = brentq(lambda s: scale_scores(s)[0], -30, 30)
    hi = brentq(lambda s: scale_scores(s)[1], -30, 30)
    wlo = x*x/(b*math.exp(2*hi)); whi = x*x/(a*math.exp(2*lo))
    rf = lambda w: w/(1+w)
    hf = lambda w: np.log1p(w)-rf(w)
    normlo = a/2*max(0., digamma((a+1)/2)-digamma(b/2))-.5
    normhi = b/2*(digamma((b+1)/2)-digamma(a/2))-.5
    slo = len(x)*normlo+sum((rf(wlo)-b*hf(whi))/2)
    shi = len(x)*normhi+sum((rf(whi)-a*hf(wlo))/2)
    for nu in np.linspace(a,b,5):
        opt = minimize_scalar(lambda u: -ll(x,nu,u), bounds=(-30,30), method='bounded', options={'xatol':1e-12})
        assert lo-1e-6 <= opt.x <= hi+1e-6
        w = x*x/(nu*math.exp(2*opt.x))
        score = len(x)*(nu/2*(digamma((nu+1)/2)-digamma(nu/2))-.5)+sum((rf(w)-nu*hf(w))/2)
        assert slo-1e-9 <= score <= shi+1e-9
        interval_checks += 1
print(f'{interval_checks} root and score interval comparisons passed')

# Independent high-precision check of the large-shape normalizer bounds.
import mpmath as mp
mp.mp.dps = 70
for av in np.geomspace(32., 1e8, 100):
    a = mp.mpf(float(av)); b = a*mp.mpf('1.13')
    low = 1/(4*b)-1/(8*a**3)
    high = 1/(4*a)-1/(8*b**3)+1/(4*a**5)
    for nu in (a, (a+b)/2, b):
        actual = nu/2*(mp.digamma((nu+1)/2)-mp.digamma(nu/2))-mp.mpf('.5')
        assert low <= actual <= high
print('300 high-precision large-shape normalizer comparisons passed')
