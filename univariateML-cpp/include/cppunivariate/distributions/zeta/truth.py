"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    s=t['s']; z=mp.zeta(s)
    out=Reference(lambda x:pair(mp.log(mp.zeta(s,x+1)/z),False),1,mp.inf,True)
    # Euler--Maclaurin evaluates the infinite tail, not a truncated body.
    out.normalization=mp.fsum(mp.exp(logd(mp.mpf(k))) for k in range(1,65))+mp.zeta(s,65)/z
    return out

