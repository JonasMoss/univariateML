"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['prob'] == 1: return atomic({0: 1}, 0)
    p=t['prob']
    out=Reference(lambda x: pair((x+1)*mp.log1p(-p),False),0,mp.inf,True)
    out.normalization=mp.mpf(1)  # Exact generating-series identity; pmf differences checked separately.
    return out

