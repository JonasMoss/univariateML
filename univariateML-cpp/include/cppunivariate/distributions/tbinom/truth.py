"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    # At probability endpoints the conditional law concentrates on an extreme.
    lo, hi = max(0,mp.floor(t['a'])+1), min(t['size'],mp.floor(t['b']))
    if lo > hi: raise ValueError('empty conditional support')
    if t['prob'] in (0, 1) or hi == lo:
        return atomic({lo if t['prob'] == 0 else hi: 1}, lo, hi)
    lo=max(0,mp.floor(t['a'])+1)
    hi=mp.floor(t['b'])
    hi=min(hi,t['size'])
    return finite_pmf(logd,lo,hi)


def boundary_adjust(t):
    # Size zero is an attained member only when the truncation includes zero.
    if t['size'] == 0: return {**t, 'a': -1., 'b': 0.}
    return {**t, 'a': -1.} if t['prob'] == 0 else t

# The weak limit exists, but the dossier excludes zero-probability
# conditioning events; retain its independent reference outside active rows.
boundary_parameter_sets = [{'prob': 0., 'a': 0., 'b': 10., 'size': 10.}]
truth_point_exclusions = [([0.0, 0.0, 10.0, 10.0],
    'TASK-134: excluded by tbinom/README.md Details: zero-probability conditioning event is an escape, not an attained member; public d/p/q/r intentionally return NaN. Use ztbinom for the attained positive-truncation boundary.')]
