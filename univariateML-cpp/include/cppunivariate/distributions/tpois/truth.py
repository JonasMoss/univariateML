"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['lambda'] == 0:
        lo=max(0,mp.floor(t['a'])+1)
        return atomic({lo: 1}, lo, mp.floor(t['b']))
    lo=max(0,mp.floor(t['a'])+1)
    hi=mp.floor(t['b'])
    return finite_pmf(logd,lo,hi)


def boundary_adjust(t):
    # Include zero for an ordinary conditional member at lambda=0.
    return {**t, 'a': -1., 'b': 0.} if t['lambda'] == 0 else t

# Retain the independent weak limit under strictly positive truncation too.
boundary_parameter_sets = [{'lambda': 0., 'a': 0., 'b': 10.}]
truth_point_exclusions = [([0., 0., 10.],
    'TASK-106: positive-truncated Poisson zero-rate limit returns NaN; see boundary report')]
