"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    r=t['lambda']
    base=lambda x: gamma(x+1,r)[::-1]
    ratio=lambda k:r/(k+1)
    weight=mp.mpf(1)
    survival0=base(0)[1]
    def tails(x):
        small=base(x)[1]+mp.log(weight)-survival0
        return pair(small,False)
    out=Reference(tails,1,mp.inf,True)
    out.normalization=geometric_sum(lambda k:mp.exp(logd(mp.mpf(k))),1,ratio)
    return out

