"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    if t['r'] == 0: return atomic({0: 1}, 0, 0)
    if t['N']>100000 and t['m']<100:
        N,m,r=t['N'],int(t['m']),int(t['r'])
        def tails(x):
            # X is the r-th marked position: X<=x iff at least r
            # of the m marked positions occur among the first x draws.
            weights=[mp.binomial(x,j)*mp.binomial(N-x,m-j)/mp.binomial(N,m)
                     if 0<=m-j<=N-x and j<=x else mp.mpf(0) for j in range(m+1)]
            return mp.log(mp.fsum(weights[r:])),mp.log(mp.fsum(weights[:r]))
        out=Reference(tails,r,r+N-m,True)
        out.normalization=mp.mpf(1)  # Partition of all m-subsets of N positions.
        return out
    return finite_pmf(logd,t['r'],t['r']+t['N']-t['m'])


# Joint/finite-design limit stresses beyond the historical grid.
extreme_parameter_sets = [{'N': 100000000}]
