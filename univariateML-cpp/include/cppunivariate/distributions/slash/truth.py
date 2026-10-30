"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s=t['mu'],t['sigma']
    def ld(x):
        z=abs((x-m)/s)
        if z==0:return -mp.log(s)-mp.log(2*mp.pi)/2-mp.log(2)
        return -mp.log(s)-mp.log(2*mp.pi)/2+mp.log(-mp.expm1(-z*z/2))-2*mp.log(z)
    def tails(x):
        z=abs((x-m)/s)
        if z==0: return (-mp.log(2),-mp.log(2))
        small=mp.exp(normal(-z)[0])-mp.expm1(-z*z/2)/(mp.sqrt(2*mp.pi)*z)
        return pair(mp.log(small),x<m)
    return Reference(tails,breaks=[m],log_density=ld)

