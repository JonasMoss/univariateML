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


truth_point_exclusions = [([0.9999999999999999], 'TASK-196 open: public CDF returns NaN at x=1.0, lower=True, log=True; independent value -3.6037789929704576. Near-unit logarithmic-series survival exceeds the geometric-series guard; a uniform bounded tail evaluator is missing.')]
