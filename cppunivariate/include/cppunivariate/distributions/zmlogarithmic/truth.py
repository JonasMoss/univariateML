"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    from functools import lru_cache
    from quality.truth_reference import logarithmic_tail
    p=t['prob']
    @lru_cache(maxsize=10000)
    def tails(x):
        k=int(x)+1
        lp=logarithmic_tail(p,k)+mp.log1p(-t['p0'])
        if lp > -mp.log(2):
            return pair(mp.log(mp.fsum(mp.exp(logd(mp.mpf(j))) for j in range(0,k))))
        return pair(lp,False)
    out=Reference(tails,0,mp.inf,True)
    out.normalization=geometric_sum(lambda k:mp.exp(logd(mp.mpf(k))),0,lambda k:p)
    return out
