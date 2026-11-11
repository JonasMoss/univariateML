"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    shape = t['shape']
    scale = t['scale']
    out = Reference(lambda x: pair(-(x/scale)**(-shape)), 0, mp.inf)
    out.chart = lambda y: (scale*mp.exp(y/shape), mp.log(scale/shape)+y/shape)
    out.unchart = lambda x: shape*mp.log(x/scale)
    out.log_density = lambda x: -mp.inf if x == 0 else mp.log(shape/scale)-(shape+1)*mp.log(x/scale)-(x/scale)**(-shape)
    return out


boundary_parameter_sets = [
    {'shape': a, 'scale': s}
    for a in (1e6, 1e9, 1e12, 1e15) for s in (1., a)
] + [{'shape': 3., 'scale': .1}]
