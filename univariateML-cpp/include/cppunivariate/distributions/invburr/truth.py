"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    shape1 = t['shape1']
    shape2 = t['shape2']
    scale = t['scale']
    return Reference(lambda x: pair(-shape1*mp.log1p((scale/x)**shape2)), 0, mp.inf)


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'shape2': 100000000.0, 'scale': 1e-08}, {'shape1': 100000000.0, 'shape2': 1e-08}]
