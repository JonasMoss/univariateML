"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    mu,sigma=t['mu'],t['sigma']
    def tails(x):
        z=(x-mu)/sigma
        return pair(mp.log(mp.erf(z/mp.sqrt(2)))) if z<1 else pair(normal(z)[1]+mp.log(2),False)
    return Reference(tails,mu)
