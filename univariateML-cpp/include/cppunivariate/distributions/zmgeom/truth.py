"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['prob'] == 1: return atomic({0: t['p0'], 1: 1-t['p0']})
    if t['p0'] == 1: return atomic({0: 1})
    p=t['prob']
    base=lambda x:pair((x+1)*mp.log1p(-p),False)
    ratio=lambda k:1-p
    weight=1-t['p0']
    survival0=base(0)[1]
    def tails(x):
        small=base(x)[1]+mp.log(weight)-survival0
        return pair(small,False)
    out=Reference(tails,0,mp.inf,True)
    out.normalization=mp.mpf(1)  # Exact generating-series identity; pmf differences checked separately.
    return out
