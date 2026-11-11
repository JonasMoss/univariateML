"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    lo, hi = t['min'], t['max']
    count = hi-lo+1
    def tails(x):
        k = mp.floor(x)
        lower = (k-lo+1)/count
        upper = (hi-k)/count
        return mp.log(lower), mp.log(upper)
    ref = Reference(tails,lo,hi,True)
    ref.normalization = mp.mpf(1)  # count equally weighted atoms
    def quantile(lp, lower):
        if lp == -mp.inf: return lo if lower else hi
        if lp == 0: return hi if lower else lo
        u = mp.exp(lp) if lower else -mp.expm1(lp)
        return max(lo,min(hi,lo-1+mp.ceil(count*u)))
    ref.quantile = quantile
    return ref


# Joint/finite-design limit stresses beyond the historical grid.
extreme_parameter_sets = [{'max': 100000000}, {'min': -100000000}]
