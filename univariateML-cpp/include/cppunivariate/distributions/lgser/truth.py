"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    from functools import lru_cache
    from quality.truth_reference import logarithmic_tail
    p=t['theta']
    @lru_cache(maxsize=10000)
    def tails(x):
        k=int(x)+1
        lp=logarithmic_tail(p,k)
        return pair(lp,False)
    out=Reference(tails,1,mp.inf,True)
    out.normalization=mp.mpf(1)  # Exact generating-series identity; pmf differences checked separately.
    return out
