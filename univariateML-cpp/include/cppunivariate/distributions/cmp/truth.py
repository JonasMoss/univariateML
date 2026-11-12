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
    if t['lambda']==1 and 0<t['nu']<mp.mpf('1e-6'):
        return unit_rate_reference(t['nu'])
    if t['nu']==1:
        a=t['lambda']
        out=Reference(lambda x:gamma(x+1,a)[::-1],0,mp.inf,True,
                      log_density=lambda x:-a+x*mp.log(a)-mp.loggamma(x+1))
        out.normalization=mp.mpf(1)  # exponential series
        return out
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


def unit_rate_reference(nu):
    """Euler--Maclaurin sum for lambda=1 and small positive nu.

    f(x)=Gamma(x+1)^(-nu). On |z-(x+1)|<=R<=x/2, x>=10000,
    |psi(z)|<=L(x)=log(2(x+1))+2. Choose R=min(x/2,1/(nu*L)).
    Thus |f(z)|<=e*f(x) and |f^(n)(x)|<=n!*e*f(x)/R^n.
    Log-concavity bounds f(x+t)<=f(x)*exp(-delta*t), delta=nu*psi(x+1).
    L(x+t)<=L(x)+t/(x+1). These give the explicit integrated Cauchy
    remainder below. No estimate is returned unless its bound closes.
    """
    from functools import lru_cache
    N=10000; order=20
    weights=[mp.exp(-nu*mp.loggamma(k+1)) for k in range(N)]
    prefix=[];total=mp.mpf(0)
    for w in weights:
        total+=w;prefix.append(total)
    @lru_cache(maxsize=20000)
    def tail(start):
        start=mp.mpf(start)
        lg=mp.loggamma(start+1);lf=-nu*lg
        ratio=mp.exp(-nu*mp.log(start+1))
        if start>mp.mpf('1e40'):
            width=-mp.log1p(-ratio)
            if width<mp.mpf('1e-45')*abs(lf):return lf+width
            raise ArithmeticError('CMP far-tail logarithmic bound')
        delta=nu*mp.digamma(start+1)
        if delta<=0:raise ArithmeticError('CMP endpoint decay sign')
        def f(u):return mp.exp(-nu*(mp.loggamma(start+1+u/delta)-lg))
        cuts=[mp.mpf(v) for v in (0,1,4,16,64,200)]
        transition=delta*(start+1)
        while transition<200:
            cuts.append(transition);transition*=10
        integral,error=mp.quad(f,sorted(set(cuts)),error=True)
        integral/=delta;error/=delta
        g=[None]+[-nu*mp.polygamma(j-1,start+1) for j in range(1,order)]
        bell=[mp.mpf(1)]
        for j in range(1,order):
            bell.append(mp.fsum(mp.binomial(j-1,k-1)*g[k]*bell[j-k] for k in range(1,j+1)))
        correction=mp.fsum(mp.bernoulli(2*j)/mp.factorial(2*j)*bell[2*j-1]
                           for j in range(1,order//2+1))
        value=integral+mp.mpf('.5')-correction
        L=mp.log(2*(start+1))+2
        moment=mp.fsum(mp.binomial(order,j)*L**(order-j)*mp.factorial(j)/
                      (delta*(start+1))**j for j in range(order+1))
        derivative=mp.factorial(order)*mp.e/delta*((2/start)**order+nu**order*moment)
        remainder=2*mp.zeta(order)/(2*mp.pi)**order*derivative
        bound=remainder+mp.exp(-200)/delta+error
        if value<=0 or bound>mp.mpf('1e-45')*value:
            raise ArithmeticError('CMP Euler--Maclaurin remainder '+mp.nstr(bound/value,10))
        return lf+mp.log(value)
    z=prefix[-1]+mp.exp(tail(N))
    @lru_cache(maxsize=20000)
    def tails(k):
        if k<N:return pair(mp.log(prefix[int(k)]/z))
        return pair(tail(k+1)-mp.log(z),False)
    ref=Reference(tails,0,mp.inf,True,
                  log_density=lambda x:-nu*mp.loggamma(x+1)-mp.log(z))
    # The positive prefix and bounded EM tail enclose this normalizer to
    # relative 1e-45; the shared verifier requires only 1e-25.
    ref.normalization=mp.mpf(1)
    ref.normalizer=z
    ref.em_tail=tail
    return ref
