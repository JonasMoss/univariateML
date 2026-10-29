"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,e=t['mu'],t['sigma'],t['epsilon']
    c=mp.sqrt(2*mp.pi)*mp.erf(e/mp.sqrt(2))+2*mp.exp(-e*e/2)/e
    def tails(x):
        z=(x-m)/s
        if abs(z)>e: small=mp.exp(e*e/2-e*abs(z))/(e*c)
        else: small=(mp.exp(-e*e/2)/e+mp.sqrt(mp.pi/2)*(mp.erf(e/mp.sqrt(2))-mp.erf(abs(z)/mp.sqrt(2))))/c
        return pair(mp.log(small),z<0)
    return Reference(tails,breaks=[m-e*s,m,m+e*s])

