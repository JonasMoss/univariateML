"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    a=t['rho']
    out=Reference(lambda x:pair(mp.loggamma(a+1)+loggamma_ratio(x,1,a+1),False),1,mp.inf,True,
                  log_density=lambda x:mp.log(a)+mp.loggamma(a+1)+loggamma_ratio(x,0,a+1))
    # Telescoping beta recurrence: sum_{k=1}^n rho B(k,rho+1)=1-n B(n,rho+1).
    # For rho>0, n B(n,rho+1) tends to zero. Evaluating the
    # limit avoids O(1/rho) doublings merely to confirm normalization.
    out.normalization=mp.mpf(1)
    return out

