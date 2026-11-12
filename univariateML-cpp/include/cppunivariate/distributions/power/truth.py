"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    alpha = t['alpha']
    beta = t['beta']
    out = Reference(lambda x: pair(beta*mp.log(x/alpha)), 0, alpha)

    out.quantile=lambda lp,lower:alpha*mp.exp((lp if lower else complement(lp))/beta)
    out.normalization_integrand=lambda y:mp.exp(-y)
    out.normalization_domain=[0,1,10,120]
    return out
