"""High precision primitives for family-local truth routes; no family registry.

A route supplies a support and BOTH log tails, obtained by integrating the
family's declarative density. The generator separately checks normalisation
and the derivative of these antiderivatives against that density.
"""
import mpmath as mp
from functools import lru_cache


def complement(logp):
    if logp < -10000:
        # Opposite log tail is below exp(-10000), hence rounds to -0 in
        # binary64. Avoid constructing an integer exponent of exp(exp(x)).
        return mp.mpf(0)
    if logp == -mp.inf:
        return mp.mpf(0)
    if logp == 0:
        return -mp.inf
    return mp.log(-mp.expm1(logp))


def pair(logp, lower=True):
    return (logp, complement(logp)) if lower else (complement(logp), logp)


def normal(z):
    if abs(z) > 1e5:
        # Mills expansion: alternating remainder < next term for z>1e5.
        terms = [mp.mpf(1)]
        for k in range(1,12): terms.append(-terms[-1]*(2*k-1)/(z*z))
        small = -z*z/2-mp.log(abs(z))-mp.log(2*mp.pi)/2+mp.log(mp.fsum(terms))
    else:
        small = mp.log(mp.erfc(abs(z)/mp.sqrt(2))/2)
    return pair(small, z < 0)


def gamma(a, z):
    if z <= 0:
        return -mp.inf, mp.mpf(0)
    lower = z<a
    def integral(is_lower):
        return mp.log(mp.gammainc(a,0,z)/mp.gamma(a) if is_lower
                      else mp.gammainc(a,z,mp.inf)/mp.gamma(a))
    lp=integral(lower)
    if lp>-mp.log(2):lower=not lower;lp=integral(lower)
    return pair(lp,lower)


def beta(a, b, z, w=None):
    if w is None:w=1-z
    if z<=0:return -mp.inf,mp.mpf(0)
    if w<=0:return mp.mpf(0),-mp.inf
    lower=z<a/(a+b)
    def integral(is_lower):
        return mp.log(mp.betainc(a,b,0,z,regularized=True) if is_lower
                      else mp.betainc(b,a,0,w,regularized=True))
    lp=integral(lower)
    if lp>-mp.log(2):lower=not lower;lp=integral(lower)
    return pair(lp,lower)


def student(df, z):
    t = df/(df+z*z)
    small = mp.log(mp.betainc(df/2, mp.mpf('.5'), 0, t, regularized=True)/2)
    return pair(small, z < 0)


def loggamma_ratio(x, a, b):
    # First omitted term bounded by O((|a|+|b|+1)^2/x). Used only
    # when that bound is < 1e-45, far below table accuracy.
    if x > mp.mpf('1e50')*(abs(a)+abs(b)+1)**2:
        return (a-b)*mp.log(x)
    return mp.loggamma(x+a)-mp.loggamma(x+b)


def binary64(v):
    try: return float(v)
    except OverflowError: return float('inf') if v>0 else -float('inf')


class Reference:
    def __init__(self, tails, lower=-mp.inf, upper=mp.inf, discrete=False,
                 breaks=(), log_density=None):
        self.tails_fn = tails
        self.lower, self.upper = mp.mpf(lower), mp.mpf(upper)
        self.discrete = discrete
        self.breaks = list(breaks)
        self.log_density = log_density

    def tails(self, x):
        if self.discrete:
            x = mp.floor(x)
            if x < self.lower:
                return -mp.inf, mp.mpf(0)
        elif x <= self.lower:
            return -mp.inf, mp.mpf(0)
        if x >= self.upper:
            return mp.mpf(0), -mp.inf
        return self.tails_fn(x)

    def chart(self, t):
        lo, hi = self.lower, self.upper
        if mp.isfinite(lo) and mp.isfinite(hi):
            z = 1/(1+mp.exp(-t))
            return lo+(hi-lo)*z, mp.log(hi-lo)-mp.log1p(mp.exp(t))-mp.log1p(mp.exp(-t))
        if mp.isfinite(lo):
            return lo+mp.exp(t), t
        if mp.isfinite(hi):
            return hi-mp.exp(-t), -t
        return mp.sinh(t), mp.log(mp.cosh(t))

    def unchart(self, x):
        lo, hi = self.lower, self.upper
        if mp.isfinite(lo) and mp.isfinite(hi):
            return mp.log(x-lo)-mp.log(hi-x)
        if mp.isfinite(lo):
            return mp.log(x-lo)
        if mp.isfinite(hi):
            return -mp.log(hi-x)
        return mp.asinh(x)

    def quantile(self, lp, lower):
        if lp == -mp.inf:
            return self.lower if lower else self.upper
        if lp == 0:
            return self.upper if lower else self.lower
        if self.lower < 0 < self.upper and self.tails(mp.mpf(0))[0 if lower else 1] == lp:
            return mp.mpf(0)
        # Root in a logarithmic support coordinate, always bracketed.
        if self.discrete:
            lo = int(self.lower) if mp.isfinite(self.lower) else -1
            hi = int(self.upper) if mp.isfinite(self.upper) else max(1, lo+1)
            def below(x):
                v = self.tails(x)[0 if lower else 1]
                return v < lp if lower else v > lp
            if not mp.isfinite(self.lower):
                while not below(lo):
                    lo = 2*lo-1
            else:
                lo -= 1
            limit = int(mp.mpf(float.fromhex('0x1.fffffffffffffp+1023'))+mp.power(2,970))
            while below(hi):
                if hi >= limit: return mp.inf
                hi = min(limit,hi*hi+1) if hi>0 else 1
            while hi-lo > 1:
                if hi > 2**53 and binary64(mp.mpf(lo+1))==binary64(mp.mpf(hi)):
                    break  # every remaining integer rounds to the same double
                mid = (lo+hi)//2
                if below(mid): lo = mid
                else: hi = mid
            return mp.mpf(hi)
        def below(t):
            x = self.chart(t)[0]
            v = self.tails(x)[0 if lower else 1]
            return v < lp if lower else v > lp
        a, b = mp.mpf(-1), mp.mpf(1)
        while not below(a) and a > -16384: a *= 2
        while below(b) and b < 16384: b *= 2
        for _ in range(220):
            mid = (a+b)/2
            if below(mid): a = mid
            else: b = mid
            if binary64(self.chart(a)[0])==binary64(self.chart(b)[0]): break
        return self.chart((a+b)/2)[0]


def finite_pmf(logpmf, lower, upper):
    """Both finite sums use positive terms; never subtract an almost-one CDF."""
    lo, hi = int(lower), int(upper)
    weights = [mp.exp(logpmf(mp.mpf(k))) for k in range(lo, hi+1)]
    def tails(x):
        k = int(x)-lo+1
        return mp.log(mp.fsum(weights[:k])), mp.log(mp.fsum(weights[k:]))
    r = Reference(tails, lo, hi, True)
    r.normalization = mp.fsum(weights)
    return r


def geometric_sum(term, start, ratio_bound, relative=mp.mpf('1e-45')):
    """Positive series. Bound every omitted term by a geometric majorant.

    ratio_bound(k) must bound term(j+1)/term(j) for ALL j>=k.
    No result is returned if a bound cannot be established in 200000 terms.
    """
    if start > mp.mpf('1e40'):
        first = term(start)
        ratio = ratio_bound(start)
        if 0 <= ratio < 1 and -mp.log1p(-ratio) < mp.mpf('1e-35')*abs(mp.log(first)):
            # Certified interval [first,first/(1-ratio)] has <1e-35
            # relative width on the log scale used in truth.json.
            return first/(1-ratio)
    total = mp.mpf(0)
    for k in range(start, start+200000):
        value = term(k)
        total += value
        ratio = ratio_bound(k)
        if 0 <= ratio < 1 and value*ratio/(1-ratio) < abs(total)*relative:
            return total
    raise ArithmeticError('positive series did not reach certified remainder')


def beta_normalization(ref, a, b):
    """Integrate a transformed beta density without subtracting its endpoints.

    y=expit(t) gives f(y)dy = exp(a log y+b log(1-y))/B(a,b) dt.
    The omitted integrals are bounded by exp(-a L)/(a B) and
    exp(-b R)/(b B), each < exp(-90). The local route establishes the
    beta change of variable; its derivative is checked against README f.
    """
    logb=mp.log(mp.beta(a,b))
    left=(90+abs(logb)+abs(mp.log(a)))/a
    right=(90+abs(logb)+abs(mp.log(b)))/b
    ref.normalization_integrand=lambda t:mp.exp(-a*mp.log1p(mp.exp(-t))-b*mp.log1p(mp.exp(t))-logb)
    ref.normalization_domain=[-left,mp.mpf(0),right]
    return ref


def logarithmic_tail(p, k):
    """Logarithmic-series survival from its positive density integral.

    Sum_{j=k}^infinity p^j/j = p^k/k integral exp(-v)/(1-p exp(-v/k)) dv.
    The omitted v>V integral is <=exp(-V)/(1-p). For huge k the integral
    lies between 1/(1-p)-p/(k(1-p)^2) and 1/(1-p).
    """
    if k>mp.mpf('1e40'):
        integral=1/(1-p)
        if p/(k*(1-p))>mp.mpf('1e-35'):raise ArithmeticError('logarithmic asymptotic bound')
    else:
        end=120-mp.log1p(-p)
        integral,error=mp.quad(lambda v:mp.exp(-v)/(-mp.expm1(mp.log(p)-v/k)),
                               [0,1,10,end],error=True)
        if error>mp.mpf('1e-45')*integral:raise ArithmeticError('logarithmic integral error')
    return k*mp.log(p)-mp.log(k)+mp.log(integral)-mp.log(-mp.log1p(-p))


@lru_cache(maxsize=50000)
def negative_binomial(r, p, x):
    """Beta integral for the NB pmf sum, with a bounded far-count tail.

    Beyond 1e40, the pmf-ratio geometric bound differs from 1-p by
    O(r/x); the omitted log correction is <1e-35 for these points.
    """
    if x>mp.mpf('1e40'):
        if abs(r-1)/(x*p)>mp.mpf('1e-35'):raise ArithmeticError('NB tail bound')
        lp=(r-1)*mp.log(p)+(x+1)*mp.log1p(-p)+loggamma_ratio(x,r+1,2)-mp.loggamma(r)
        return pair(lp,False)
    if x>300:
        logfirst=r*mp.log(p)+(x+1)*mp.log1p(-p)+loggamma_ratio(x,r+1,2)-mp.loggamma(r)
        term=mp.mpf(1); total=mp.mpf(1)
        for j in range(200000):
            ratio=(1-p)*(x+r+1+j)/(x+2+j)
            majorant=max(1-p,ratio)
            if majorant<1 and term*majorant/(1-majorant)<mp.mpf('1e-45')*total:
                return pair(logfirst+mp.log(total),False)
            term*=ratio;total+=term
        raise ArithmeticError('NB positive tail series remainder')
    return beta(r,x+1,p)
