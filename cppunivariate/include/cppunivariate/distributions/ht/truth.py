"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    nu,sigma=t['nu'],t['sigma']
    def tails(x):
        z=(x/sigma)**2
        return beta(mp.mpf('.5'),nu/2,z/(nu+z),nu/(nu+z))
    return Reference(tails,0)
