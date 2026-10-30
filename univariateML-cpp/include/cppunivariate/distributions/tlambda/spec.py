"""Implicit inverse and analytic implicit derivatives for the quality harness.

Numerical evaluation is independent of the C++ inversion: SciPy for the oracle,
mpmath Newton refinement for the high precision log likelihood.
"""
import sympy as sp
import mpmath as mp
import numpy as np
from scipy.stats import tukeylambda


def inverse(z, shape):
    if isinstance(z, mp.mpf) or isinstance(shape, mp.mpf):
        z, shape = mp.mpf(z), mp.mpf(shape)
        if shape == 0:
            return 1 / (1 + mp.exp(-z))
        u = mp.mpf(float(tukeylambda.cdf(float(z), float(shape))))
        if not 0 < u < 1:
            return u
        for _ in range(15):
            q = (mp.expm1(shape * mp.log(u)) - mp.expm1(shape * mp.log1p(-u))) / shape
            d = u**(shape - 1) + (1-u)**(shape - 1)
            v = u - (q-z)/d
            if not 0 < v < 1:
                break
            if abs(v-u) < mp.mpf('1e-32'):
                return v
            u = v
        return u
    return tukeylambda.cdf(z, shape)


class U(sp.Function):
    nargs = 2
    _imp_ = staticmethod(inverse)

    def fdiff(self, argindex=1):
        z, a = self.args
        u = U(z, a)
        d = u**(a-1) + (1-u)**(a-1)
        if argindex == 1:
            return 1/d
        qa = ((u**a*sp.log(u) - (1-u)**a*sp.log(1-u))*a - (u**a-(1-u)**a))/a**2
        return -qa/d


def log_density(symbols, x):
    mu, sigma, a = [symbols[k] for k in ('mu', 'sigma', 'lambda')]
    u = U((x-mu)/sigma, a)
    return -sp.log(sigma) - sp.log(u**(a-1) + (1-u)**(a-1))
