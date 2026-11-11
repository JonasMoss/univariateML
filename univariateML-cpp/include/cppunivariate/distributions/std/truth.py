"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    mean = t['mean']
    sd = t['sd']
    nu = t['nu']
    return Reference(lambda x: student(nu,(x-mean)/sd*mp.sqrt(nu/(nu-2))), -mp.inf, mp.inf)


# Approach nu=2 at fixed ordinary Student scale.
extreme_parameter_sets = [{'nu': 2.00000001, 'sd': 14142.135659086289}]
