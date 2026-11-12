"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    from quality.truth_reference import gamma_power_reference
    return gamma_power_reference(t['shape'],2,mp.sqrt(t['scale']/t['shape']),False)
