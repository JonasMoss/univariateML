"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import conditioned_positive_tails
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['lambda'] == 0: return atomic({1: 1}, 1)
    r=t['lambda']
    base=lambda x: gamma(x+1,r)[::-1]
    ratio=lambda k:r/(k+1)
    weight=mp.mpf(1)
    zero=base(0)
    def tails(x):
        return conditioned_positive_tails(base(x),zero)
    out=Reference(tails,1,mp.inf,True)
    out.normalization=mp.mpf(1)  # Exact generating-series identity; pmf differences checked separately.
    return out

