"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t):
    from quality.truth_reference import gamma_power_reference
    return gamma_power_reference(t['shape1'],t['shape2'],t['scale'],True)


# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'shape1': 1e-08, 'shape2': 100000000.0}]

# Coupled power-limit overflow with an overflow-safe product coordinate.
extreme_parameter_sets.append({'shape1': 1e-309, 'shape2': 1e+308, 'scale': 1.0})

archive_001625 = {'shape1': 8.615406571602086e22,
                  'shape2': float.fromhex('0x1.faf47fa66b243p+0'),
                  'scale': 424695594989.53754}
extreme_parameter_sets.extend([archive_001625, {'shape1': 1e30, 'shape2': 2.0, 'scale': 1.0}])

# The truth harness samples binary64 quantiles. Supply the probabilities
# at the five archive observations so these exact x values appear in it.
with mp.workdps(80):
    archive_reference = build({name: mp.mpf(value) for name, value in archive_001625.items()})
    quantile_inputs = [
        (True, float(archive_reference.tails(mp.mpf(float.fromhex(x)))[0]))
        for x in ('0x1.1cd03c7e16dbfp+0', '0x1.1cd03c7e18152p+0', '0x1.1cd03c7e194e4p+0',
                  '0x1.1cd03c7e1a876p+0', '0x1.1cd03c7e1cf9bp+0')
    ]
