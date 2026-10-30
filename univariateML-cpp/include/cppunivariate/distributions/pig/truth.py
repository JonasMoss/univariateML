"""Bessel-K masses, geometric count bounds, and a large-order integral.

K_nu(z)=Gamma(nu)/2*(z/2)^(-nu) E[exp(-c/T)], T~Gamma(nu),
c=z^2/4. Taylor's integral remainder bounds the alternating expansion
by c^J Gamma(nu-J)/(J! Gamma(nu)), for J<nu. For count tails, each
term can be summed by Euler's beta integral; after v=-n log(t),
the normalized integral is exp(-v) h(v)^(3/2) times the same polynomial
with c replaced by c*h(v), h=(1-A)/(1-A exp(-v/n))<=1.
Thus the same next Taylor term bounds its error. v>160 contributes
<=exp(-160)*sum(abs(coefficients)); quadrature error is checked too.
For small orders use the exact half-integer K recurrence. Since K_nu
increases with |nu| (its cosh integral is monotone), the count ratio for k>=1 is bounded by
A+max(C-3*A/2,0)/(k+1), where A=2*m^2/(s+2*m^2), C=m/sqrt(1+2*m^2/s).
This decreases, giving a geometric remainder. No unbounded truncation.

The inherited far-count gamma-ratio approximation has absolute log error
<=2*|u-v|*(max(|u|,|v|)+1)/x for x>2*max(|u|,|v|). Integrate
log(y)-1/y < digamma(y) < log(y) between the shifts to obtain this bound.
The helper's threshold makes it <2e-50 per ratio.
"""
from functools import lru_cache
import mpmath as mp
from quality.truth_reference import Reference, pair, loggamma_ratio


def build(p, logd):
    m,s=p['mean'],p['shape']
    z=mp.sqrt(s*(2+s/m**2)); c=z*z/4
    A=2*m*m/(s+2*m*m); C=m/mp.sqrt(1+2*m*m/s)
    constant=mp.log(mp.sqrt(s/(2*mp.pi)))+s/m-mp.log(A)/2
    logs=[s/m-z]; ratios=[C]
    def extend(k):
        while len(logs)<=k:
            j=len(logs)
            ratio=C if j==1 else A*(1-mp.mpf('1.5')/j)+C*C/(j*(j-1)*ratios[-1])
            logs.append(logs[-1]+mp.log(ratio));ratios.append(ratio)
    def coefficients(n):
        result=[mp.mpf(1)]
        for j in range(1,200):
            if n-mp.mpf('.5')-j<=0:raise ArithmeticError('Bessel Taylor order')
            nxt=-result[-1]*c/(j*(n-mp.mpf('.5')-j))
            if abs(nxt)<mp.mpf('1e-50'):return result,abs(nxt)
            result.append(nxt)
        raise ArithmeticError('Bessel Taylor remainder')
    cutoff=max(300,4*c)
    body_upper={}
    @lru_cache(maxsize=20000)
    def density(k):
        if k<cutoff:
            extend(int(k));return logs[int(k)]
        cs,err=coefficients(k)
        total=mp.fsum(cs)
        if err>abs(total)*mp.mpf('1e-45'):raise ArithmeticError('Bessel density remainder')
        return constant+k*mp.log(A)+loggamma_ratio(k,-mp.mpf('.5'),1)+mp.log(total)
    @lru_cache(maxsize=20000)
    def upper(n):
        if n in body_upper:return body_upper[n]
        if n<cutoff:
            extend(int(n));first=logs[int(n)];total=mp.mpf(1);term=total;k=int(n)
            while True:
                bound=A+max(C-mp.mpf('1.5')*A,0)/(k+1)
                if bound<1 and term*bound/(1-bound)<total*mp.mpf('1e-46'):break
                k+=1;extend(k);term=mp.exp(logs[k]-first);total+=term
            return first+mp.log(total)
        cs,err=coefficients(n)
        def integrand(v):
            h=(1-A)/((1-A)-A*mp.expm1(-v/n))
            return mp.exp(-v)*h**mp.mpf('1.5')*mp.polyval(list(reversed(cs)),h)
        if n>mp.mpf('1e50'):
            # |h-1|<A*v/(n*(1-A)); integral log error <1e-35.
            if (len(cs)+2)*A/(n*(1-A))>mp.mpf('1e-35'):raise ArithmeticError('far PIG bound')
            value=mp.fsum(cs)
        else:
            value,error=mp.quad(integrand,[0,1,4,16,64,160],error=True)
            bound=err+mp.exp(-160)*mp.fsum(abs(t) for t in cs)
            if error+bound>value*mp.mpf('1e-40'):raise ArithmeticError('PIG integral remainder')
        return constant+n*mp.log(A)+loggamma_ratio(n,-mp.mpf('.5'),1)-mp.log1p(-A)+mp.log(value)
    @lru_cache(maxsize=20000)
    def tails(k):
        if k<100:
            extend(int(k));ll=mp.log(mp.fsum(mp.exp(v) for v in logs[:int(k)+1]))
            if ll<-mp.log(2):return pair(ll)
        return pair(upper(k+1),False)
    for k in (0,1,7):
        if abs(density(mp.mpf(k))-logd(mp.mpf(k)))>mp.mpf('1e-50'):
            raise ArithmeticError('README density mismatch')
    ref=Reference(tails,0,mp.inf,True,log_density=density)
    # One certified tail at the upper body edge supplies every earlier
    # suffix by positive addition. Its relative remainder bound can only
    # improve as masses are added; no repeated long summation is needed.
    edge=int(mp.ceil(cutoff))
    extend(edge-1)
    total=mp.exp(upper(mp.mpf(edge)))
    for k in range(edge-1,-1,-1):
        total+=mp.exp(logs[k]);body_upper[k]=mp.log(total)
    ref.normalization=total
    return ref
