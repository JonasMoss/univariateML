"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import beta_normalization, Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    shape1 = t['shape1']
    shape2 = t['shape2']
    shape3 = t['shape3']
    scale = t['scale']
    ref = Reference(lambda x: beta(shape1,shape2,(x/scale)**shape3), 0, scale)

    return beta_normalization(ref,shape1,shape2)

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'shape1': 100000000.0, 'shape3': 1e-08}, {'shape1': 1e-08, 'shape3': 100000000.0}]


def endpoint_log_density(t, x):
    """Exact one-sided power-law endpoint, before symbolic Gamma evaluation."""
    a,b,c,s=t['shape1'],t['shape2'],t['shape3'],t['scale']
    if x==0:
        power=a*c-1
        return -mp.inf if power>0 else mp.inf if power<0 else mp.log(c)-mp.log(s)-mp.log(mp.beta(a,b))
    if x==s:
        return -mp.inf if b>1 else mp.inf if b<1 else mp.log(a)+mp.log(c)-mp.log(s)
    return None
