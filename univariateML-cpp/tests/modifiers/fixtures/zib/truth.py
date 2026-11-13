"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    # At probability endpoints the conditional law concentrates on an extreme.
    lo, hi = 0, t['size']
    if t['size'] == 0 or t['pi'] == 1 or t['prob'] == 0: return atomic({0: 1}, lo, hi)
    if t['prob'] == 1: return atomic({0: t['pi'], hi: 1-t['pi']}, lo, hi)
    if t['size'] > 100000:
        from quality.truth_reference import binomial_reference
        return binomial_reference(t['size'],t['prob'],1-t['pi'],False)
    return finite_pmf(logd,0,t['size'])

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'size': 100000000, 'prob': 1e-08}]
