"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    rate = t['lambda']
    r = Reference(lambda x: gamma(x+1, rate)[::-1], 0, mp.inf, True)
    r.normalization = geometric_sum(lambda k: mp.exp(-rate)*rate**k/mp.factorial(k), 0, lambda k: rate/(k+1))
    return r

