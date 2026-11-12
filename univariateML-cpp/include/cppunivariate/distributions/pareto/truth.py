"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    a = t['a']
    b = t['b']
    out = Reference(lambda x: pair(a*mp.log(b/x),False), b, mp.inf)
    out.chart=lambda y:(b*mp.exp(y/a),mp.log(b/a)+y/a)
    out.unchart=lambda x:a*mp.log(x/b)
    out.quantile=lambda lp,lower:b*mp.exp(-(complement(lp) if lower else lp)/a) if lp not in (-mp.inf,0) else (out.lower if (lp==-mp.inf)==lower else out.upper)
    out.normalization_integrand=lambda y:mp.exp(-y)
    out.normalization_domain=[0,1,10,120]
    return out
