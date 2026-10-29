"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    lo=max(0,mp.floor(t['a'])+1)
    hi=mp.floor(t['b'])
    return finite_pmf(logd,lo,hi)

