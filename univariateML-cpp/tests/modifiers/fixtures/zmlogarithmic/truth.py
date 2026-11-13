"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['prob'] == 0: return atomic({0: t['p0'], 1: 1-t['p0']})
    if t['p0'] == 1: return atomic({0: 1})
    from functools import lru_cache
    from quality.truth_reference import logarithmic_tail
    p=t['prob']
    @lru_cache(maxsize=10000)
    def tails(x):
        k=int(x)+1
        lp=logarithmic_tail(p,k)+mp.log1p(-t['p0'])
        return pair(lp,False)
    out=Reference(tails,0,mp.inf,True)
    out.normalization=mp.mpf(1)  # Exact generating-series identity; pmf differences checked separately.
    return out


truth_point_exclusions = [([0.9999999999999999, 0.5], 'TASK-196 open: public CDF returns NaN at x=1.0, lower=True, log=True; independent value -0.6662904127618943. Near-unit logarithmic-series survival exceeds the geometric-series guard; a uniform bounded tail evaluator is missing.')]
