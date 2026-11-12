"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd=None):
    mean = t['mean']
    sd = t['sd']
    nu = t['nu']
    out=Reference(lambda x: student(nu,(x-mean)/sd*mp.sqrt(nu/(nu-2))), -mp.inf, mp.inf)
    out.log_density=logd
    degrees=t['nu']
    C=mp.exp(mp.loggamma((degrees+1)/2)-mp.loggamma(degrees/2)-mp.log(degrees*mp.pi)/2)
    # Complete Student beta integral after the location-scale map.
    out.normalization_identity=C*mp.sqrt(degrees)*mp.beta(mp.mpf('.5'),degrees/2)
    return out


# Approach nu=2 at fixed ordinary Student scale.
extreme_parameter_sets = [{'nu': 2.00000001, 'sd': 14142.135659086289}]
