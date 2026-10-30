"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    shape = t['shape']
    scale = t['scale']
    min = t['min']
    return Reference(lambda x: pair(-shape*mp.log1p((x-min)/scale),False), min, mp.inf)

