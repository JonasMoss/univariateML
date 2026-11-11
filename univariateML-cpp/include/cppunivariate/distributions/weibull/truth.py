"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    shape = t['shape']
    scale = t['scale']
    ref = Reference(lambda x: pair(-(x/scale)**shape,False), 0, mp.inf)
    # y=(x/scale)^shape has unit exponential density. This chart
    # remains integrable when the x quantiles overflow binary64.
    ref.normalization_integrand = lambda y: mp.exp(-y)
    ref.normalization_domain = [0, 1, mp.inf]
    def quantile(lp, lower):
        if lp == -mp.inf: return mp.mpf(0) if lower else mp.inf
        if lp == 0: return mp.inf if lower else mp.mpf(0)
        hazard = -complement(lp) if lower else -lp
        return scale*mp.exp(mp.log(hazard)/shape)
    ref.quantile = quantile
    return ref


# Nonunit scale makes amplified log-ratio cancellation observable.
extreme_parameter_sets = [{'shape': 1e12, 'scale': 1000.}]

# Unit-hazard probabilities expose small-shape inverse amplification.
quantile_inputs = [(False,0.6321205588285577),(False,0.36787944117144233),
                   (True,-0.4586751453870819),(True,-1.)]
