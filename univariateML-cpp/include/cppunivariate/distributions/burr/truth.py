"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    from quality.truth_reference import burr_reference
    return burr_reference(t['shape1'], t['shape2'], t['scale'], 0, False)


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'shape2': 100000000.0, 'scale': 100000000.0}, {'shape1': 100000000.0, 'shape2': 1e-08}]
