"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['r'] == 0: return atomic({0: 1}, 0, 0)
    return finite_pmf(logd,t['r'],t['r']+t['N']-t['m'])

