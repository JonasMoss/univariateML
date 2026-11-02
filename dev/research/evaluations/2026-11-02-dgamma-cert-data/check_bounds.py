#!/usr/bin/env python3
"""Numerical checks of the TASK-116 inequalities; not a proof."""
import math
import numpy as np
from scipy.integrate import quad
from scipy.optimize import brentq
from scipy.special import digamma, gammainc, gammaln
from scipy.stats import gamma

rng = np.random.default_rng(11620261102)
count = 0
for _ in range(300):
    x = rng.integers(0, 12, size=30)
    x[0], x[1] = 0, 11
    values, weights = np.unique(x, return_counts=True)
    mean = np.mean(x)
    a = math.exp(rng.uniform(math.log(.02), math.log(80)))
    def lp(k, r):
        # Use whichever tail avoids subtracting two values close to one.
        lo, hi = r*k, r*(k+1)
        if gammainc(a, hi) < .5:
            p = gamma.cdf(hi, a) - gamma.cdf(lo, a)
        else:
            p = gamma.sf(lo, a) - gamma.sf(hi, a)
        return math.log(p) if p > 0 else -math.inf
    def score(r):
        z = 0.
        for k, w in zip(values, weights):
            l = lp(k, r)
            if not math.isfinite(l):
                return math.nan
            hi, lo = r*(k+1), r*k
            f = lambda t: math.exp(a*math.log(t)-t-gammaln(a)-l) if t else 0.
            z += w*(f(hi)-f(lo))
        return z
    rlo, rhi = a/(mean+1), a/mean
    r = brentq(score, rlo, rhi, xtol=1e-13)
    loglik = sum(w*lp(k,r) for k,w in zip(values,weights))
    c = min(math.sqrt(11)-1-math.log(math.sqrt(11)),
            1/math.sqrt(11)-1+math.log(math.sqrt(11)))
    assert loglik <= -a*c*min(weights[0],weights[-1])+1e-8
    if a <= 1:
        assert loglik <= weights[-1]*math.log(2*math.e*a*math.log1p(1/11))+1e-8
    # Direct quadrature of the zero-cell conditional log moment.
    denom = quad(lambda y: a*y**(a-1)*math.exp(-r*y),0,1)[0]
    numer = quad(lambda y: a*y**(a-1)*math.exp(-r*y)*math.log(y),0,1)[0]
    assert -math.log(max(1.,r))-math.e/a-1e-7 <= numer/denom <= 0
    assert rlo-1e-12 <= r <= rhi+1e-12
    def likelihood_at(t):
        aa = math.exp(t)
        total = 0.
        for k, w in zip(values, weights):
            lo, hi = r*k, r*(k+1)
            if gammainc(aa, hi) < .5:
                p = gamma.cdf(hi, aa) - gamma.cdf(lo, aa)
            else:
                p = gamma.sf(lo, aa) - gamma.sf(hi, aa)
            total += w*math.log(p)
        return total / len(x)
    t, step = math.log(a), 1e-5
    slope = (likelihood_at(t+step)-likelihood_at(t-step))/(2*step)
    bound = a*(abs(math.log(r))+abs(digamma(a))+
               max(math.log(values[-1]+1), math.log(max(1.,r))+math.e/a))
    assert abs(slope) <= bound + 1e-6
    count += 1
print(f'{count} randomized checks passed; seed 11620261102')
