"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import beta_normalization, Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd=None):
    shape1 = t['shape1']
    shape2 = t['shape2']
    scale = t['scale']
    ref = Reference(lambda x: beta(shape1,shape2,x/(scale+x),scale/(scale+x)), 0, mp.inf)

    ref.log_density=logd
    return beta_normalization(ref,shape1,shape2)

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'shape2': 100000000.0, 'scale': 100000000.0}, {'shape1': 100000000.0, 'scale': 1e-08}, {'shape1': 100000000.0, 'shape2': 100000000.0}]


# Independently measured production limitations; numerical rows remain archived.
truth_exclusions = {}
truth_exclusions.setdefault('values', []).extend([([100000000.0, 100000000.0, 1.0], [0.9947744473451168], 'TASK-196 open: measured double,native p normalized error 1.3733142638245321e-08 (log outputs use abs(error)/max(1,abs(reference)); natural tails use relative error). The inherited large-shape beta tail needs a verified relative-error remainder.'), ([100000000.0, 100000000.0, 1.0], [1.0052530024960225], 'TASK-196 open: measured double,native p normalized error 1.372575295378571e-08 (log outputs use abs(error)/max(1,abs(reference)); natural tails use relative error). The inherited large-shape beta tail needs a verified relative-error remainder.')])
