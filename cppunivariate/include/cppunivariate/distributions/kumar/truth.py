"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, beta_normalization, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    a = t['a']
    b = t['b']
    # y=x**a has the Beta(1,b) density. Its logistic chart avoids
    # subtraction at x=1 when b<1; beta_normalization bounds omitted mass.
    ref = Reference(lambda x: pair(b*mp.log1p(-x**a),False), 0, 1)
    return beta_normalization(ref,mp.mpf(1),b)

