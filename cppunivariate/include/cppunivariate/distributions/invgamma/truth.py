"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    alpha = t['alpha']
    beta = t['beta']
    return Reference(lambda x: gamma(alpha, beta/x)[::-1], 0, mp.inf)

