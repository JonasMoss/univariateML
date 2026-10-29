"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    m,s,xi=t['mean'],t['sd'],t['xi']
    a=t['nu']
    m1=mp.gamma(2/a)/mp.sqrt(mp.gamma(1/a)*mp.gamma(3/a))
    b=mp.sqrt(mp.gamma(1/a)/mp.gamma(3/a))
    base=lambda z:pair(gamma(1/a,(abs(z)/b)**a)[1]-mp.log(2),z<0)
    shift=m1*(xi-1/xi)
    scale=mp.sqrt((1-m1*m1)*(xi*xi+1/(xi*xi))+2*m1*m1-1)
    def tails(x):
        y=scale*(x-m)/s+shift
        if y<0: return pair(mp.log(2/(1+xi*xi))+base(y*xi)[0])
        return pair(mp.log(2*xi*xi/(1+xi*xi))+base(y/xi)[1],False)
    return Reference(tails,breaks=[m-s*shift/scale])

