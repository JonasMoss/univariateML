"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    b,eta=t['b'],t['eta']
    def tails(x):
        y=mp.exp(-b*x)
        lower=mp.log(-mp.expm1(-b*x))-eta*y
        if lower < -mp.log(2):return pair(lower)
        upper=mp.log(-mp.expm1(-eta*y)+y*mp.exp(-eta*y))
        return pair(upper,False)
    return Reference(tails,0)
