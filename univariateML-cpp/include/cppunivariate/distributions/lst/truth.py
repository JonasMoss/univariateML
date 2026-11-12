"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd=None):
    mu = t['mu']
    sigma = t['sigma']
    df = t['df']
    out=Reference(lambda x: student(df,(x-mu)/sigma), -mp.inf, mp.inf)
    out.log_density=logd
    degrees=t['df']
    C=mp.exp(mp.loggamma((degrees+1)/2)-mp.loggamma(degrees/2)-mp.log(degrees*mp.pi)/2)
    # Complete Student beta integral after the location-scale map.
    out.normalization_identity=C*mp.sqrt(degrees)*mp.beta(mp.mpf('.5'),degrees/2)
    return out

