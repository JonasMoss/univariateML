"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    # At probability endpoints the conditional law concentrates on an extreme.
    lo, hi = 0, t['size']
    if lo > hi: raise ValueError('empty conditional support')
    if t['prob'] in (0, 1) or hi == lo:
        return atomic({lo if t['prob'] == 0 else hi: 1}, lo, hi)
    return finite_pmf(logd,0,t['size'])


def boundary_adjust(t):
    # Exercise the attained size cap on the exact atomic submodel, avoiding
    # ten million mpmath terms. This does not cover its nondegenerate members.
    return {**t, 'prob': 0.} if t['size'] == 10000000 else t
