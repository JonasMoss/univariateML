"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    shape = t['shape']
    scale = t['scale']
    ref = Reference(lambda x: pair(-mp.log1p((x/scale)**shape),False), 0, mp.inf)
    # u=F(x) transforms the declared density to the unit uniform law.
    ref.normalization_integrand = lambda u: mp.mpf(1)
    ref.normalization_domain = [0, 1]
    def quantile(lp, lower):
        if lp == -mp.inf: return mp.mpf(0) if lower else mp.inf
        if lp == 0: return mp.inf if lower else mp.mpf(0)
        logit = lp-complement(lp)
        return scale*mp.exp((logit if lower else -logit)/shape)
    ref.quantile = quantile
    return ref


# Nonunit scale makes amplified log-ratio cancellation observable.
extreme_parameter_sets = [{'shape': 1e12, 'scale': 1000.}]
