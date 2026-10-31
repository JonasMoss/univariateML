"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

def expression(symbols,x):
    import sympy as sp
    return sp.S.Zero  # exact log series evaluated below, not golden spec

def build(t):
    if t['lambda'] == 0: return atomic({0: 1})
    if t['nu'] == 0:
        if t['lambda'] >= 1: raise ValueError('nu=0 requires lambda<1 for normalization')
        a=t['lambda']
        out=Reference(lambda x:pair((x+1)*mp.log(a),False),0,mp.inf,True,
                      log_density=lambda x:mp.log1p(-a)+x*mp.log(a))
        out.normalization=mp.mpf(1)
        return out
    a,b=t['lambda'],t['nu']
    from functools import lru_cache
    @lru_cache(maxsize=100000)
    def weight(k): return mp.exp(k*mp.log(a)-b*mp.loggamma(k+1))
    ratio=lambda k:a/(k+1)**b
    z=geometric_sum(weight,0,ratio)
    ld=lambda x:x*mp.log(a)-b*mp.loggamma(x+1)-mp.log(z)
    @lru_cache(maxsize=10000)
    def tails(x):
        k=int(x)
        if x>mp.mpf('1e40'):
            # S lies between first term and first/(1-ratio).
            # Bound the error on the stored LOG value, not natural mass.
            lp=ld(x+1); width=-mp.log1p(-ratio(x+1))
            if width < mp.mpf('1e-35')*abs(lp): return pair(lp,False)
        if k<a**(1/b):return pair(mp.log(mp.fsum(weight(j) for j in range(k+1))/z))
        return pair(mp.log(geometric_sum(weight,k+1,ratio)/z),False)
    out=Reference(tails,0,mp.inf,True,log_density=ld)
    out.normalization=geometric_sum(lambda k:mp.exp(ld(mp.mpf(k))),0,ratio)
    return out


def boundary_adjust(t):
    # The nu=0 face is geometric only on lambda<1.
    return {**t, 'lambda': .5} if t['nu'] == 0 and t['lambda'] >= 1 else t
