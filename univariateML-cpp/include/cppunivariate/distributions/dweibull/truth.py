"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    p,b=t['shape1'],t['shape2']
    def ld(x):
        if x==0:return mp.log1p(-p)
        power=x**b
        delta=power*mp.expm1(b*mp.log1p(1/x))
        return power*mp.log(p)+complement(delta*mp.log(p))
    out=Reference(lambda x:pair((x+1)**b*mp.log(p),False),0,mp.inf,True,log_density=ld)
    # Telescoping sum of p^(k^b)-p^((k+1)^b); exact residual.
    n=mp.ceil((mp.log(mp.mpf('1e-45'))/mp.log(p))**(1/b))
    remainder_log=(n+1)**b*mp.log(p)
    out.normalization=mp.mpf(1) if remainder_log<mp.log(mp.eps) else -mp.expm1(remainder_log)
    return out

