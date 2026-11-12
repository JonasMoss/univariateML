"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    m,s=t['mean'],t['sd']
    def ld(x):
        a,b=(x-m)/s,(x+1-m)/s
        if a>mp.mpf('1e30'):return normal(a)[1]  # omitted opposite endpoint < exp(-a/s)
        if a>0: return mp.log(mp.erfc(a/mp.sqrt(2))-mp.erfc(b/mp.sqrt(2)))-mp.log(2)
        return mp.log(mp.erfc(-b/mp.sqrt(2))-mp.erfc(-a/mp.sqrt(2)))-mp.log(2)
    out=Reference(lambda x:normal((x+1-m)/s),-mp.inf,mp.inf,True,log_density=ld)
    out.normalization=mp.mpf(1)  # Telescoping normal bins / two geometric series.
    return out

