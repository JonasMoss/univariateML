"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,a,b=t['mean'],t['sd'],t['a'],t['b']
    def interval(l,h):
        if l>=0:
            u,v=normal(l)[1],normal(h)[1]
            return u+mp.log(-mp.expm1(v-u)) if v!=-mp.inf else u
        if h<=0:
            u,v=normal(h)[0],normal(l)[0]
            return u+mp.log(-mp.expm1(v-u)) if v!=-mp.inf else u
        return mp.log((mp.erf(h/mp.sqrt(2))-mp.erf(l/mp.sqrt(2)))/2)
    z=interval((a-m)/s,(b-m)/s)
    def tails(x):
        l=interval((a-m)/s,(x-m)/s)-z
        h=interval((x-m)/s,(b-m)/s)-z
        return pair(l) if l<h else pair(h,False)
    return Reference(tails,a,b)
