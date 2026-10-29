"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    sigma=t['sigma']
    return Reference(lambda x:pair(mp.log(2/mp.pi*mp.atan(x/sigma))) if x<=sigma
                     else pair(mp.log(2/mp.pi*mp.atan(sigma/x)),False),0)
