"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    rate, shape = t['lambda'], t['kappa']
    return Reference(lambda x: pair(-shape*mp.log1p(rate*x), False), 0)


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'kappa': 100000000.0, 'lambda': 1e-08}]
