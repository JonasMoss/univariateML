"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    from quality.truth_reference import gamma_power_reference
    return gamma_power_reference(t['shape1'],t['shape2'],t['scale'],False)


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'shape1': 1e-08, 'shape2': 100000000.0}]

# Coupled power-limit overflow with an overflow-safe product coordinate.
extreme_parameter_sets.append({'shape1': 1e-309, 'shape2': 1e+308, 'scale': 1.0})
