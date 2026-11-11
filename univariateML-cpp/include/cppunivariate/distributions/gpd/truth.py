"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

boundary_parameters = {'xi': [-1.]}
special_points = {'xi': [-5e-10, 0., 5e-10]}
def build(t):
    m,s,a=t['mu'],t['sigma'],t['xi']
    hi = m-s/a if a<0 else mp.inf
    out = Reference(lambda x: pair(-(x-m)/s if a==0 else -mp.log1p(a*(x-m)/s)/a,False), m, hi)

    def chart(v):
        y=mp.exp(v)
        x=m+s*y if a==0 else m+s*mp.expm1(a*y)/a
        return x,mp.log(s)+a*y+v
    out.chart=chart
    out.unchart=lambda x:mp.log((x-m)/s if a==0 else mp.log1p(a*(x-m)/s)/a)
    return out

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'xi': 1e-12}, {'xi': -1e-12}]
