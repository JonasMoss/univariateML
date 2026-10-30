"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s = t['mu'],t['sigma']
    return Reference(lambda x: pair(-abs(x-m)/s-mp.log(2), x<m), breaks=[m])

