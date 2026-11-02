#!/usr/bin/env python3
"""Quadrature check of conditional-moment and secant bounds; not a proof."""
import math
import numpy as np
from scipy.integrate import quad
from scipy.special import gammaln
from scipy.stats import gamma

rng = np.random.default_rng(11620261103)
x = np.array([0, 0, 0, 1, 2, 4, 7])
for trial in range(100):
    A = math.exp(rng.uniform(math.log(.3), math.log(6)))
    B = A * math.exp(rng.uniform(.001, .15))
    R = math.exp(rng.uniform(math.log(.3), math.log(4)))
    S = R * math.exp(rng.uniform(.001, .15))
    a, r = rng.uniform(A,B), rng.uniform(R,S)
    h = A * .001
    def H(aa, rr):
        vals = []
        for k in x:
            lo, hi = rr*k, rr*(k+1)
            p = (gamma.cdf(hi,aa)-gamma.cdf(lo,aa) if gamma.cdf(hi,aa)<.5
                 else gamma.sf(lo,aa)-gamma.sf(hi,aa))
            vals.append(math.log(p))
        return np.mean(vals) + gammaln(aa) - aa*math.log(rr)
    def moments(aa, rr):
        logs, ys = [], []
        for k in x:
            f = lambda y: y**(aa-1)*math.exp(-rr*y)
            den = quad(f,k,k+1,epsabs=1e-12,epsrel=1e-11)[0]
            logs.append(quad(lambda y: f(y)*math.log(y),k,k+1,epsabs=1e-12,epsrel=1e-10)[0]/den)
            ys.append(quad(lambda y: f(y)*y,k,k+1,epsabs=1e-12,epsrel=1e-11)[0]/den)
        return np.mean(logs), np.mean(ys)
    logmean, ymean = moments(a,r)
    lower = (H(A,S)-H(A-h,S))/h
    upper = (H(B+h,R)-H(B,R))/h
    assert lower-1e-7 <= logmean <= upper+1e-7, (trial, lower, logmean, upper)
    _, ya = moments(A,r)
    _, yb = moments(B,r)
    g = a-r*ymean
    assert B-r*yb-(B-A)-1e-10 <= g <= A-r*ya+(B-A)+1e-10
print('100 conditional-moment enclosure checks passed; seed 11620261103')
