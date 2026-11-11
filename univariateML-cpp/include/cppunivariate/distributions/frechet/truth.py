"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

# mu is the actual lower support endpoint, although README calls it location.
held_parameters = ['mu']

def build(t):
    a, m, s = t['lambda'], t['mu'], t['sigma']
    out = Reference(lambda x: pair(-((x-m)/s)**(-a)), m)
    # Log-distance scaled by shape resolves the concentrated density in quadrature.
    out.chart = lambda y: (m+s*mp.exp(y/a), mp.log(s/a)+y/a)
    out.unchart = lambda x: a*mp.log((x-m)/s)
    # The endpoint limit is zero for every finite positive shape; bypass
    # expensive symbolic endpoint limits with million/integer exponents.
    out.log_density = lambda x: -mp.inf if x == m else mp.log(a/s)-(a+1)*mp.log((x-m)/s)-((x-m)/s)**(-a)
    return out


# Exact binary64 coefficients, including nonzero and inexact mu+sigma.
boundary_parameter_sets = [
    {'lambda': a, 'mu': -a + offset, 'sigma': a}
    for a in (1e6, 1e9, 1e12, 1e15) for offset in (0., .25, 2.)
] + [{'lambda': 3., 'mu': .1, 'sigma': 1.},
     {'lambda': 1e12, 'mu': 1., 'sigma': 1e12},
     {'lambda': 1e15, 'mu': .1, 'sigma': 1.},
     {'lambda': 1e15, 'mu': -.1, 'sigma': 1.}]
