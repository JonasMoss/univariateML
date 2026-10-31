"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['pi'] == 1: return atomic({0: 1})
    r=t['lambda']
    base=lambda x: gamma(x+1,r)[::-1]
    ratio=lambda k:r/(k+1)
    weight=1-t['pi']
    survival0=base(0)[1]
    def tails(x):
        small=base(x)[1]+mp.log(weight)
        return pair(small,False)
    out=Reference(tails,0,mp.inf,True)
    out.normalization=geometric_sum(lambda k:mp.exp(logd(mp.mpf(k))),0,ratio)
    return out

