"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

# mu is the actual lower support endpoint, although README calls it location.
held_parameters = ['mu']

def build(t):
    a, m, s = t['lambda'], t['mu'], t['sigma']
    return Reference(lambda x: pair(-((x-m)/s)**(-a)), m)

