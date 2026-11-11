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
    ref=finite_pmf(logd,0,t['size'])
    ordinary=ref.quantile
    def quantile(lp,lower):
        # Symmetry resolves an exact median even when every interior mass
        # is below the working precision relative to the endpoint masses.
        if t['prob']==mp.mpf('.5') and lp==mp.log(mp.mpf('.5')):
            return mp.floor(t['size']/2)
        return ordinary(lp,lower)
    ref.quantile=quantile
    return ref


def boundary_adjust(t):
    # Exercise the attained size cap on the exact atomic submodel, avoiding
    # ten million mpmath terms. This does not cover its nondegenerate members.
    return {**t, 'prob': 0.} if t['size'] == 10000000 else t

# Joint/finite-design limit stresses beyond the historical grid.
extreme_parameter_sets = [{'nu': 100000000.0}, {'nu': -100000000.0}, {'size': 10000, 'prob': 0.0001, 'nu': 1}]
