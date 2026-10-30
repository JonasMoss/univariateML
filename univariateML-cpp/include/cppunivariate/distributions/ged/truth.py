"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,a=t['mean'],t['sd'],t['nu']
    scale=s*mp.sqrt(mp.gamma(1/a)/mp.gamma(3/a))
    def tails(x):
        small=gamma(1/a,(abs(x-m)/scale)**a)[1]-mp.log(2)
        return pair(small,x<m)
    return Reference(tails,breaks=[m])

