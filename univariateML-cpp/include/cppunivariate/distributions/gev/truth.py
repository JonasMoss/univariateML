"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

boundary_parameters = {'xi': [-1.]}
special_points = {'xi': [-5e-10, 0., 5e-10]}
def build(t):
    m,s,a=t['mu'],t['sigma'],t['xi']
    lo = m-s/a if a>0 else -mp.inf
    hi = m-s/a if a<0 else mp.inf
    out = Reference(lambda x: pair(-mp.exp(-(x-m)/s if a==0 else -mp.log1p(a*(x-m)/s)/a)), lo,hi)

    def chart(y):
        x=m+s*y if a==0 else m+s*mp.expm1(a*y)/a
        return x,mp.log(s)+a*y
    out.chart=chart
    out.unchart=lambda x:(x-m)/s if a==0 else mp.log1p(a*(x-m)/s)/a
    def ld(x):
        if x==hi and a == -1: return -mp.log(s)
        if x==lo or x==hi:return -mp.inf
        y=out.unchart(x)
        return -mp.log(s)-(1+a)*y-mp.exp(-y)
    out.log_density=ld
    return out
