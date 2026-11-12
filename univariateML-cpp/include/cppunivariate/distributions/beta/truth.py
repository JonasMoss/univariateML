"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import beta_normalization, Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd=None):
    shape1 = t['shape1']
    shape2 = t['shape2']
    ref = Reference(lambda x: beta(shape1,shape2,x), 0, 1)

    ref.log_density=logd
    return beta_normalization(ref,shape1,shape2)

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'shape1': 100000000.0, 'shape2': 100000000.0}]


# Independently measured production limitations; numerical rows remain archived.
truth_exclusions = {}
truth_exclusions.setdefault('values', []).extend([([100000000.0, 100000000.0], [0.5013098104053413], 'TASK-196 open: measured double,native p normalized error 1.3770772917461083e-08 (log outputs use abs(error)/max(1,abs(reference)); natural tails use relative error). A uniform large-shape incomplete-beta tail with a verified relative-error remainder is missing.')])
