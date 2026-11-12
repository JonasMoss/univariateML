"""High precision primitives for family-local truth routes; no family registry.

A route supplies a support and BOTH log tails, obtained by integrating the
family's declarative density. The generator separately checks normalisation
and the derivative of these antiderivatives against that density.
"""
import mpmath as mp
from math import isqrt
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
    return mp.log1p(-mp.exp(logp)) if logp < -mp.log(2) else mp.log(-mp.expm1(logp))


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


@lru_cache(maxsize=20000)
def concentrated_gamma(a,z):
    """Exact shifted gamma integral, scaled at the requested endpoint.

    Log-concavity bounds the omitted integral by its endpoint tangent.
    This is quadrature of the density, not a normal approximation.
    """
    lower=z<a
    direction=-1 if lower else 1
    width=min(mp.sqrt(a),z/max(1,abs(a-1-z)))
    limit=z/width if lower else mp.inf
    def logkernel(v):
        u=direction*width*v/z
        return (a-1)*mp.log1p(u)-direction*width*v if u>-1 else -mp.inf
    end=min(mp.mpf(16),limit)
    for _ in range(20):
        if end==limit:break
        slope=direction*width*((a-1)/(z+direction*width*end)-1)
        if slope<0 and logkernel(end)-mp.log(-slope)<-180:break
        end=min(end*2,limit)
    else:raise ArithmeticError('gamma tangent tail bound')
    cuts=sorted(set([mp.mpf(0),end]+[mp.mpf(v) for v in (1,4,16,64) if v<end]))
    value,error=mp.quad(lambda v:mp.exp(logkernel(v)),cuts,method='gauss-legendre',maxdegree=8,error=True)
    if error>mp.mpf('1e-50')*value:raise ArithmeticError('centered gamma quadrature error')
    lp=(a-1)*mp.log(z)-z-mp.loggamma(a)+mp.log(width*value)
    return pair(lp,lower)


def gamma(a, z):
    if z <= 0:
        return -mp.inf, mp.mpf(0)
    if z<mp.mpf('1e-50'):
        # P(a,z)=z**a/Gamma(a+1) E[exp(-z*U)], U~Beta(a,1).
        # 1-z <= E[exp(-z*U)] <= 1; omitted log width < 2e-50.
        # Direct upper gammainc tries an enormous 2F0 argument 1/z here.
        return pair(a*mp.log(z)-mp.loggamma(a+1))
    if z>mp.mpf('1e50')*(1+abs(a-1)):
        # Gamma(a,z)=z^(a-1) exp(-z) integral exp(-t)(1+t/z)^(a-1) dt.
        # log1p(t/z)<=t/z encloses the integral between 1 and
        # 1/(1-(a-1)/z), with reversed bounds when a<1. Log width<1e-50.
        return pair((a-1)*mp.log(z)-z-mp.loggamma(a),False)
    if a>=1 and a==mp.floor(a) and z>=2*a:
        # Exact finite upper-gamma polynomial for positive integer shape.
        # Its remaining positive terms have a decreasing ratio <=(a-j-1)/z.
        term=mp.mpf(1);total=term
        for j in range(1,1001):
            term*=(a-j)/z;total+=term
            ratio=max(mp.mpf(0),(a-j-1)/z)
            if term==0 or term*ratio/(1-ratio)<total*mp.mpf('1e-100'):
                return pair((a-1)*mp.log(z)-z-mp.loggamma(a)+mp.log(total),False)
        raise ArithmeticError('integer gamma positive polynomial remainder')
    if a>100000 and a/2<z<2*a:return concentrated_gamma(a,z)
    lower = z<a
    def integral(is_lower):
        return mp.log(mp.gammainc(a,0,z)/mp.gamma(a) if is_lower
                      else mp.gammainc(a,z,mp.inf)/mp.gamma(a))
    lp=integral(lower)
    if lp>-mp.log(2):lower=not lower;lp=integral(lower)
    return pair(lp,lower)


def beta_fraction(a,b,z):
    """Euler incomplete-beta continued fraction (modified Lentz).

    Evaluate the smaller tail only; require 100 stable decimal digits.
    Exhaustion is a reference failure, never a returned approximation.
    """
    qab=a+b; qap=a+1; qam=a-1
    tiny=mp.mpf('1e-1000');c=mp.mpf(1);d=1-qab*z/qap
    if abs(d)<tiny:d=tiny
    d=1/d;h=d
    for m in range(1,100001):
        m2=2*m
        aa=m*(b-m)*z/((qam+m2)*(a+m2))
        d=1+aa*d;c=1+aa/c
        if abs(d)<tiny:d=tiny
        if abs(c)<tiny:c=tiny
        d=1/d;h*=d*c
        aa=-(a+m)*(qab+m)*z/((a+m2)*(qap+m2))
        d=1+aa*d;c=1+aa/c
        if abs(d)<tiny:d=tiny
        if abs(c)<tiny:c=tiny
        d=1/d;delta=d*c;h*=delta
        if abs(delta-1)<mp.mpf('1e-100'):
            return h
    raise ArithmeticError('incomplete beta continued fraction exhausted 100000 steps')


def beta(a, b, z, w=None):
    if w is None:w=1-z
    if z<=0:return -mp.inf,mp.mpf(0)
    if w<=0:return mp.mpf(0),-mp.inf
    if a==b and z==w==mp.mpf('.5'):return -mp.log(2),-mp.log(2)
    if max(a,b)>100000:
        lower=z<(a+1)/(a+b+2)
        aa,bb,zz,ww=(a,b,z,w) if lower else (b,a,w,z)
        logpref=aa*mp.log(zz)+bb*mp.log(ww)-mp.log(mp.beta(aa,bb))
        lp=logpref+mp.log(beta_fraction(aa,bb,zz)/aa)
        return pair(lp,lower)
    lower=True if w==1 else False if z==1 else z<a/(a+b)
    def integral(is_lower):
        return mp.log(mp.betainc(a,b,0,z,regularized=True) if is_lower
                      else mp.betainc(b,a,0,w,regularized=True))
    lp=integral(lower)
    if lp>-mp.log(2) and z!=1 and w!=1:
        lower=not lower;lp=integral(lower)
    return pair(lp,lower)


def student(df, z):
    t = df/(df+z*z)
    small = beta(df/2, mp.mpf('.5'), t)[0]-mp.log(2)
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
                # Exact lattice jumps can differ by a final mp rounding bit
                # after log/exp or finite summation. This guard is far below
                # binary64 input resolution, and relative even for tiny log p.
                if abs(v-lp) <= abs(lp)*mp.eps**mp.mpf('.75'):
                    return False
                return v < lp if lower else v > lp
            if not mp.isfinite(self.lower):
                while not below(lo):
                    lo = 2*lo-1
            else:
                lo -= 1
            limit = int(mp.mpf(float.fromhex('0x1.fffffffffffffp+1023'))+mp.power(2,970))
            while below(hi):
                if hi >= limit: return mp.inf
                lo = hi  # retain the last proved lower bracket endpoint
                hi = min(limit,(2*hi+1 if hi<1000000 else hi*hi+1)) if hi>0 else 1
            while hi-lo > 1:
                if hi > 2**53 and binary64(mp.mpf(lo+1))==binary64(mp.mpf(hi)):
                    break  # every remaining integer rounds to the same double
                # Counts can span hundreds of decades. Bisect their log
                # scale until the bracket has a moderate ratio, then integers.
                mid = isqrt(lo*hi) if lo>0 and hi>4*lo else (lo+hi)//2
                if below(mid): lo = mid
                else: hi = mid
            return mp.mpf(hi)
        def below(t):
            x = self.chart(t)[0]
            v = self.tails(x)[0 if lower else 1]
            return v < lp if lower else v > lp
        a, b = mp.mpf(-1), mp.mpf(1)
        while not below(a): a *= 2
        while below(b): b *= 2
        mid=(a+b)/2
        for _ in range(512):
            x,jac=self.chart(mid)
            value=self.tails(x)[0 if lower else 1]
            if value<lp if lower else value>lp:a=mid
            else:b=mid
            candidate=(a+b)/2
            if self.log_density is not None and mp.isfinite(value):
                residual=value-lp
                if abs(residual)<mp.mpf('1e-35')*max(1,abs(lp)):
                    delta=mp.mpf('1e-35')*max(1,abs(mid))
                    if below(mid-delta) and not below(mid+delta):
                        return self.chart(mid)[0]
                log_slope=self.log_density(x)+jac-value
                # Extreme trial points can have log density -exp(1e10).
                # Skip an unhelpful Newton proposal; keep the valid bracket.
                if -10000 < log_slope < 10000:
                    slope=mp.exp(log_slope)*(1 if lower else -1)
                    newton=mid-residual/slope
                    margin=(b-a)/4
                    if a+margin<newton<b-margin:candidate=newton
            xa, xb = self.chart(a)[0], self.chart(b)[0]
            rounded = binary64(xa)
            # Verification uses unrounded quantiles as integration cuts. Two
            # zeros/infinities (or a rounded finite support edge) do not locate
            # that cut: retain the logarithmic coordinate until it is resolved.
            if (not getattr(self, 'high_precision_quantile', False) and
                rounded == binary64(xb) and rounded != 0 and
                mp.isfinite(rounded) and self.lower < rounded < self.upper):
                break
            mid=candidate
        else:
            xa,xb=self.chart(a)[0],self.chart(b)[0]
            if abs(xb-xa)>mp.mpf('1e-25')*max(abs(xa),abs(xb)):
                raise ArithmeticError('reference quantile bracket did not converge')
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
    if min(a,b)>100000:
        center=mp.log(a/b);width=mp.sqrt(1/a+1/b)
        logb=mp.log(mp.beta(a,b))
        def logkernel(u):
            t=center+width*u
            return -a*mp.log1p(mp.exp(-t))-b*mp.log1p(mp.exp(t))-logb+mp.log(width)
        # Log-concavity gives the tangent-line bound on each omitted tail.
        end=mp.mpf(40)
        for direction in (-1,1):
            t=center+direction*width*end
            slope=width*(a-(a+b)/(1+mp.exp(-t)))
            if direction*slope>=0 or mp.exp(logkernel(direction*end))/abs(slope)>mp.mpf('1e-40'):
                raise ArithmeticError('centered beta normalization tail bound')
        ref.normalization_integrand=lambda u:mp.exp(logkernel(u))
        ref.normalization_domain=[-end,-10,0,10,end]
        return ref
    logb=mp.log(mp.beta(a,b))
    left=(90+abs(logb)+abs(mp.log(a)))/a
    right=(90+abs(logb)+abs(mp.log(b)))/b
    ref.normalization_integrand=lambda t:mp.exp(-a*mp.log1p(mp.exp(-t))-b*mp.log1p(mp.exp(t))-logb)
    ref.normalization_domain=[-left,mp.mpf(0),right]
    return ref


def logarithmic_stieltjes_tail(p,k):
    """Euler--Maclaurin with a signed next-term bound from coth's poles.

    1/(1-exp(-t))=1/t+1/2+2*t*sum_n 1/(t*t+4*pi*pi*n*n).
    The finite geometric expansion of each positive denominator has a
    signed remainder bounded by its next term, for every t>0.
    Integrate against exp(-k*t) from -log(p) to infinity.
    """
    x=-k*mp.log(p)
    total=mp.exp(x)*mp.e1(x)+mp.mpf('.5')/k
    term=mp.mpf(1);poly=term
    for n in range(1,129):
        term*=x/n;poly+=term
        if n%2:
            j=(n+1)//2
            correction=mp.bernoulli(2*j)/(2*j)*poly/k**(2*j)
            if abs(correction)<mp.mpf('1e-50')*abs(total):
                return k*mp.log(p)+mp.log(total)-mp.log(-mp.log1p(-p))
            total+=correction
    raise ArithmeticError('logarithmic Stieltjes next-term bound')


def logarithmic_tail(p, k):
    """Logarithmic-series survival from its positive density integral.

    Sum_{j=k}^infinity p^j/j = p^k/k integral exp(-v)/(1-p exp(-v/k)) dv.
    The omitted v>V integral is <=exp(-V)/(1-p). For huge k the integral
    lies between 1/(1-p)-p/(k(1-p)^2) and 1/(1-p).
    """
    if p>mp.mpf('.5') and k<=mp.mpf('1e40'):
        if k<200:
            prefix=mp.fsum(p**j/j for j in range(1,int(k)))
            return complement(mp.log(prefix)-mp.log(-mp.log1p(-p)))
        return logarithmic_stieltjes_tail(p,k)
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
    if r==1:return pair((x+1)*mp.log1p(-p),False)
    if r>100000:return beta(r,x+1,p)
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


def atomic(weights, lower=0, upper=None):
    """Exact atomic law; collapse an unbounded count support to its last atom.

    The weights are closed-form probabilities or parameter limits, not
    numerical approximations to nearby interior parameters.
    """
    weights = {mp.mpf(k): mp.mpf(v) for k, v in weights.items() if v != 0}
    if upper is None: upper = max(weights)
    def tails(x):
        return (mp.log(mp.fsum(v for k, v in weights.items() if k <= x)),
                mp.log(mp.fsum(v for k, v in weights.items() if k > x)))
    ref = Reference(tails, lower, upper, True,
                    log_density=lambda x: mp.log(weights.get(x, 0)))
    ref.normalization = mp.fsum(weights.values())
    ref.atoms = sorted(weights)
    original = ref.quantile
    def quantile(lp, lower):
        if lp in (-mp.inf, 0): return original(lp, lower)
        for k in sorted(weights):
            tail = ref.tails(k)[0 if lower else 1]
            if (tail >= lp if lower else tail <= lp): return k
        raise ArithmeticError('atomic quantile not bracketed')
    ref.quantile = quantile
    return ref


def burr_reference(a, b, s, m=0, inverse=False):
    """Burr integral in cumulative-hazard coordinate y=a log(1+z^b).

    Its transformed density is exp(-y), so y cuts [1e-40,120] omit
    less than 1.0001e-40 mass. The analytic physical density is still
    differentiated against the antiderivative by the shared verifier.
    """
    sign = -1 if inverse else 1
    def tails(x):
        v=sign*b*mp.log((x-m)/s)
        hazard=a*(max(v,0)+mp.log1p(mp.exp(-abs(v))))
        return pair(-hazard, inverse)
    def logx(y): return sign*mp.log(mp.expm1(y/a))/b
    def chart(t):
        # log hazard makes very small/large shapes equally well scaled.
        y=mp.exp(t); z=logx(y)
        jac=mp.log(s)+z+t-mp.log(a*b)-mp.log(-mp.expm1(-y/a))
        return m+s*mp.exp(z),jac
    def unchart(x):
        v=sign*b*mp.log((x-m)/s)
        return mp.log(a*(max(v,0)+mp.log1p(mp.exp(-abs(v)))))
    def density(x):
        if x==m:
            if inverse:
                power=a*b-1
                return mp.inf if power<0 else -mp.inf if power>0 else mp.log(a*b/s)
            if b==1:return mp.log(a/s)
            return mp.inf if b<1 else -mp.inf
        z=mp.log((x-m)/s);v=sign*b*z
        return mp.log(a*b/s)+(sign*b-1)*z-(a+1)*(max(v,0)+mp.log1p(mp.exp(-abs(v))))
    ref=Reference(tails,m,log_density=density)
    ref.chart,ref.unchart=chart,unchart
    ref.normalization_integrand=lambda y:mp.exp(-y)
    ref.normalization_domain=[mp.mpf('1e-40'),1,10,120]
    def quantile(lp,lower):
        if lp in (-mp.inf,0):return ref.lower if (lp==-mp.inf)==lower else ref.upper
        y=-(lp if lower==inverse else complement(lp))
        return m+s*mp.exp(logx(y))
    ref.quantile=quantile
    return ref


def binomial_reference(n, p, weight=1, truncate=False):
    """Incomplete-beta polynomial identity, with zero modification if requested."""
    base=lambda x: beta(n-x,x+1,1-p,p)
    lz=complement(n*mp.log1p(-p)) if truncate else mp.mpf(0)
    def tails(x):
        return pair(base(x)[1]+mp.log(weight)-lz,False)
    out=Reference(tails,1 if truncate and weight==1 else 0,n,True)
    out.normalization=mp.mpf(1)  # Binomial theorem and conditional/mixture mass.
    return out


def gamma_power_reference(a,c,s,inverse=False):
    """Gamma power transform in a scaled latent log coordinate.

    For a<1 the left log tail has scale 1/a; for a>=1 its central
    width is 1/sqrt(a). Integrate the exact transformed density.
    Omitted mass is checked by the independent incomplete-gamma tails.
    """
    sign=-1 if inverse else 1
    center=mp.log(a);width=1/a if a<1 else 1/mp.sqrt(a)
    def chart(y):
        z=(sign*center+width*y)/c
        return s*mp.exp(z),mp.log(s*width/c)+z
    def unchart(x):return (c*mp.log(x/s)-sign*center)/width
    def density(x):
        if x==0:
            if inverse:return -mp.inf
            if a*c==1:return mp.log(c/s)-mp.loggamma(a)
            return mp.inf if a*c<1 else -mp.inf
        lx=mp.log(x/s);lz=sign*c*lx
        return mp.log(c/s)-mp.loggamma(a)+(sign*c*a-1)*lx-mp.exp(lz)
    ref=Reference(lambda x:gamma(a,mp.exp(sign*c*mp.log(x/s)))[::sign],0,log_density=density)
    ref.chart,ref.unchart=chart,unchart
    # Chernoff bounds: P(G<=a exp(-120/a))<=exp(-120+a),
    # P(G>=a+120+sqrt(240a))<=exp(-at least 40).
    low=-max(mp.mpf(120)/a,mp.mpf(40)/mp.sqrt(a))/width
    high=mp.log((a+1000+mp.sqrt(2000*a))/a)/width
    low_logz=center+width*low
    high_logz=center+width*high
    for lz in (low_logz,high_logz):
        bound=a*(1-mp.exp(lz-center)+(lz-center))
        if bound>mp.log(mp.mpf('1e-40')):
            raise ArithmeticError('gamma normalization Chernoff bound')
    def integrand(y):
        v=width*y
        return mp.exp(a*v-a*mp.expm1(v)+a*center-a-mp.loggamma(a)+mp.log(width))
    ref.normalization_integrand=integrand
    ref.normalization_domain=sorted(set([low,high]+[mp.mpf(v) for v in (-10,-1,0,1,10) if low<v<high]))
    return ref


def conditioned_positive_tails(tails, zero):
    """Stable tails of a count law conditioned on X>0."""
    ll,lu=tails;l0,u0=zero
    if ll<-mp.log(2):
        return pair(ll+complement(l0-ll)-u0)
    return pair(lu-u0,False)
