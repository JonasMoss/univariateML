"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import beta_normalization, Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd=None):
    shape1 = t['shape1']
    shape2 = t['shape2']
    shape3 = t['shape3']
    scale = t['scale']
    ref = Reference(lambda x: beta(shape3,shape1,(x/scale)**shape2/(1+(x/scale)**shape2),1/(1+(x/scale)**shape2)), 0, mp.inf)

    ref.log_density=logd
    return beta_normalization(ref,shape3,shape1)

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'shape1': 100000000.0, 'scale': 100000000.0}, {'shape3': 100000000.0, 'scale': 1e-08}, {'shape1': 100000000.0, 'shape3': 100000000.0, 'shape2': 0.0001414213562373095}, {'shape1': 1e-08, 'shape2': 100000000.0}, {'shape3': 1e-08, 'shape2': 100000000.0}, {'shape1': 1e-08, 'shape3': 1e-08, 'shape2': 100000000.0}]


def endpoint_log_density(t, x):
    """Exact one-sided power-law endpoint, before symbolic Gamma evaluation."""
    b,c,a,s=t['shape1'],t['shape2'],t['shape3'],t['scale']
    edge=mp.mpf(0)
    if x==edge:
        power=a*c-1
        return -mp.inf if power>0 else mp.inf if power<0 else mp.log(c)-mp.log(s)-mp.log(mp.beta(a,b))
    return None
