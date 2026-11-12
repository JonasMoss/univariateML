"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import negative_binomial, Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['size'] == 0 or t['prob'] == 1: return atomic({0: 1})
    r,p=t['size'],t['prob']
    out=Reference(lambda x: negative_binomial(r,p,x),0,mp.inf,True)
    out.normalization=mp.mpf(1)  # Exact generating-series identity; pmf differences checked separately.
    return out


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'size': 100000000.0, 'prob': 0.9999999900000001}]
