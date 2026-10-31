"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    # At probability endpoints the conditional law concentrates on an extreme.
    lo, hi = 0, t['size']
    if t['size'] == 0 or t['pi'] == 1 or t['prob'] == 0: return atomic({0: 1}, lo, hi)
    if t['prob'] == 1: return atomic({0: t['pi'], hi: 1-t['pi']}, lo, hi)
    return finite_pmf(logd,0,t['size'])


# Independent rows retained outside active checks; exact findings in the TASK-106 report.
truth_exclusions = {'quantiles': [([10.0, 0.0, 0.5], [1, 1, 0.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([10.0, 0.0, 0.5], [1, 0, 1.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([10.0, 0.0, 0.5], [0, 1, '-Inf'],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([10.0, 0.0, 0.5], [0, 0, 0.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([10.0, 0.5, 1.0], [1, 1, 0.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([10.0, 0.5, 1.0], [1, 0, 1.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([10.0, 0.5, 1.0], [0, 1, '-Inf'],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([10.0, 0.5, 1.0], [0, 0, 0.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report')]}
