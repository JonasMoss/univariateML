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
    if not mp.isfinite(hi) or hi-lo>100000:
        raise ArithmeticError('finite conditional reference needs a bounded window of at most 100000 cells')
    # Direct MP products define the conditional law without constructing a
    # symbolic factorial or subtracting two large parent log normalizers.
    magnitudes=[abs(mp.log10(v)) for v in (t['size'],t['prob']) if v]
    mp.mp.dps=max(mp.mp.dps,int(max(magnitudes+[0]))+100)
    n,p=t['size'],t['prob'];weights=[mp.mpf(1)]
    for k in range(int(lo)+1,int(hi)+1):weights.append(weights[-1]*(n-k+1)*p/(k*(1-p)))
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
    # Size zero is an attained member only when the truncation includes zero.
    if t['size'] == 0: return {**t, 'a': -1., 'b': 0.}
    return {**t, 'a': -1.} if t['prob'] == 0 else t

# The weak limit exists, but the dossier excludes zero-probability
# conditioning events; retain its independent reference outside active rows.
boundary_parameter_sets = [{'prob': 0., 'a': 0., 'b': 10., 'size': 10.}]
truth_point_exclusions = [([0.0, 0.0, 10.0, 10.0],
    'TASK-134: excluded by tbinom/README.md Details: zero-probability conditioning event is an escape, not an attained member; public d/p/q/r intentionally return NaN. Use ztbinom for the attained positive-truncation boundary.')]

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'size': 100000000, 'prob': 1e-08}]

# Two-cell limit whose half-CDF displacement is smaller than binary64.
extreme_parameter_sets.append({"size": float.fromhex("0x1p1022"), "prob": float.fromhex("0x1p-1022"), "a": -1., "b": 1.})
