"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    a = t['a']
    b = t['b']
    return Reference(lambda x: pair(-a/b*mp.expm1(b*x),False), 0, mp.inf)

