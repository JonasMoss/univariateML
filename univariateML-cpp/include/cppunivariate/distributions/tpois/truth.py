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
    if not mp.isfinite(hi) or hi-lo>100000:
        raise ArithmeticError('finite conditional reference needs a bounded window of at most 100000 cells')
    # Direct MP products define the conditional law without constructing a
    # symbolic factorial or subtracting two large parent log normalizers.
    weights=[mp.mpf(1)]
    for k in range(int(lo)+1,int(hi)+1):weights.append(weights[-1]*t['lambda']/k)
    total=mp.fsum(weights);weights=[v/total for v in weights]
    def tails(x):
        k=max(0,min(len(weights),int(mp.floor(x)-lo+1)))
        return mp.log(mp.fsum(weights[:k])),mp.log(mp.fsum(weights[k:]))
    def density(x):
        return mp.log(weights[int(x-lo)]) if lo<=x<=hi and x==mp.floor(x) else -mp.inf
    result=Reference(tails,lo,hi,True,log_density=density)
    result.normalization=mp.fsum(weights)
    return result


def boundary_adjust(t):
    # Include zero for an ordinary conditional member at lambda=0.
    return {**t, 'a': -1., 'b': 0.} if t['lambda'] == 0 else t

# Retain the independent weak limit under strictly positive truncation too.
boundary_parameter_sets = [{'lambda': 0., 'a': 0., 'b': 10.}]
truth_point_exclusions = [([0.0, 0.0, 10.0],
    'TASK-134: excluded by tpois/README.md Details: zero-probability conditioning event is an escape, not an attained member; public d/p/q/r intentionally return NaN. Use ztpois for the attained positive-truncation boundary.')]
