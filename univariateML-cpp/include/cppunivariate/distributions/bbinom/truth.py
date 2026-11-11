"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    return finite_pmf(logd,0,t['size'])


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'alpha': 100000000.0, 'beta': 100000000.0}, {'alpha': 1e-08, 'beta': 1e-08}]
