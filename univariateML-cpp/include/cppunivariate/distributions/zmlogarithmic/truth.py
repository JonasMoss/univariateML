"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['prob'] == 0: return atomic({0: t['p0'], 1: 1-t['p0']})
    if t['p0'] == 1: return atomic({0: 1})
    from functools import lru_cache
    from quality.truth_reference import logarithmic_tail
    p=t['prob']
    @lru_cache(maxsize=10000)
    def tails(x):
        k=int(x)+1
        lp=logarithmic_tail(p,k)+mp.log1p(-t['p0'])
        if lp > -mp.log(2):
            return pair(mp.log(mp.fsum(mp.exp(logd(mp.mpf(j))) for j in range(0,k))))
        return pair(lp,False)
    out=Reference(tails,0,mp.inf,True)
    out.normalization=geometric_sum(lambda k:mp.exp(logd(mp.mpf(k))),0,lambda k:p)
    return out

# Independent rows retained outside active checks; exact findings in the TASK-106 report.
truth_exclusions = {'quantiles': [([0.5, 0.0], [1, 1, '-Inf'],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([0.5, 0.0], [1, 0, 0.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([0.5, 0.0], [0, 1, 0.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report'),
               ([0.5, 0.0], [0, 0, 1.0],
                'TASK-106 finite-support endpoint convention differs from the declared support; see boundary report')]}
