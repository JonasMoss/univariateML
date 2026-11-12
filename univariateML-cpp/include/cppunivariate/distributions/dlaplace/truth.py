"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    m,p=t['location'],t['scale']
    def tails(x):
        return pair((m-x)*mp.log(p)-mp.log1p(p)) if x<m else pair((x-m+1)*mp.log(p)-mp.log1p(p),False)
    out=Reference(tails,-mp.inf,mp.inf,True)
    out.normalization=mp.mpf(1)  # Telescoping normal bins / two geometric series.
    return out

