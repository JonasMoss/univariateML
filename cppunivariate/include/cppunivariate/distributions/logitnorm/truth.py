"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    mu = t['mu']
    sigma = t['sigma']
    ref = Reference(lambda x: normal((mp.log(x)-mp.log1p(-x)-mu)/sigma), 0, 1)

    # y=(logit(x)-mu)/sigma transforms the README integral to N(0,1).
    # Omitted normal tail at |y|=15 is < 4e-51.
    ref.normalization_integrand=lambda y:mp.exp(-y*y/2)/mp.sqrt(2*mp.pi)
    ref.normalization_domain=[mp.mpf(-15),mp.mpf(0),mp.mpf(15)]
    return ref
