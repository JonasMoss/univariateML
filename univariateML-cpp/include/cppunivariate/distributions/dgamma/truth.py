"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def build(t, logd):
    a,r=t['shape'],t['rate']
    def ld(x):
        if a==1:return -r*x+mp.log(-mp.expm1(-r))
        if x>mp.mpf('1e40')*max(1,abs(a-1)):
            # Integrating exp(-r v)*(1+v/x)^(a-1), 0<=v<=1:
            # omitted relative correction <= O(|a-1|/x) <1e-40.
            return (a-1)*mp.log(r)-mp.loggamma(a)+(a-1)*mp.log(x)-r*x+mp.log(-mp.expm1(-r))
        return mp.log(mp.gammainc(a,r*x,r*(x+1))/mp.gamma(a))
    out=Reference(lambda x:gamma(a,r*(x+1)),0,mp.inf,True,log_density=ld)
    # Exact telescoping integral, with omitted residual independently bounded.
    n=mp.ceil(max(1,a)/r*1000)
    out.normalization=mp.gammainc(a,0,r*(n+1))/mp.gamma(a)
    return out

