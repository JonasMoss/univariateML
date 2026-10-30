"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,a=t['mu'],t['sigma'],t['lambda']
    def tails(x):
        z=(x-m)/s
        p=normal(z)
        other=-a*(x-m)+(a*s)**2/2+normal(z-a*s)[0]
        if p[0]<-mp.log(2): return pair(p[0]+mp.log1p(-mp.exp(other-p[0])))
        return pair(mp.log(mp.exp(p[1])+mp.exp(other)),False)
    return Reference(tails)

