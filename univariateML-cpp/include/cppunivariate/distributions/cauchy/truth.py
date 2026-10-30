"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s=t['location'],t['scale']
    def tails(x):
        z=(x-m)/s
        small=mp.log(mp.atan(1/abs(z))/mp.pi) if z else -mp.log(2)
        return pair(small,z<0)
    return Reference(tails)

