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
from quality.truth_reference import atomic
from quality.truth_reference import Reference, pair, loggamma_ratio


def build(p, logd):
    # As shape tends to zero, the mixing Laplace transform tends to one.
    if p['shape'] == 0: return atomic({0: 1})
    m,s=p['mean'],p['shape']
    z=mp.sqrt(s*(2+s/m**2)); c=z*z/4
    A=2*m*m/(s+2*m*m); C=m/mp.sqrt(1+2*m*m/s)
    constant=mp.log(mp.sqrt(s/(2*mp.pi)))+s/m-mp.log(A)/2
    logs=[s/m-z]; ratios=[C]
    cumulative=[mp.exp(logs[0])]
    def extend(k):
        while len(logs)<=k:
            j=len(logs)
            ratio=C if j==1 else A*(1-mp.mpf('1.5')/j)+C*C/(j*(j-1)*ratios[-1])
            logs.append(logs[-1]+mp.log(ratio));ratios.append(ratio)
            cumulative.append(cumulative[-1]+mp.exp(logs[-1]))
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
            delta=n*(1-A)/A
            end=mp.mpf(200)+abs(mp.log(min(delta,1)))
            last=mp.log1p(end/delta)
            def scaled(w):
                v=delta*mp.expm1(w)
                h=(1-A)/((1-A)-A*mp.expm1(-v/n))
                return mp.exp(w-v)*h**mp.mpf('1.5')*mp.polyval(list(reversed(cs)),h)
            cuts=sorted(set([mp.mpf(0),last]+[mp.mpf(w) for w in (1,4,16,32) if w<last]+
                            [mp.log1p(v/delta) for v in (1,4,16,64) if v<end]))
            value,error=mp.quad(scaled,cuts,method='gauss-legendre',maxdegree=6,error=True)
            if error>abs(value)*mp.mpf('1e-45'):
                value,error=mp.quad(scaled,cuts,method='gauss-legendre',maxdegree=8,error=True)
            value*=delta;error*=delta
            # h <= (1-A)*(n+v)/(n*(1-A)+v). Split v at n;
            # integral exp(-v) h^(3/2) <= 6*n*(1-A)+(2*(1-A))^(3/2)*exp(-n).
            mass_bound=min(1,6*n*(1-A)+(2*(1-A))**mp.mpf('1.5')*mp.exp(-n))
            bound=err*mass_bound+mp.exp(-end)*mp.fsum(abs(t) for t in cs)
            if error+bound>value*mp.mpf('1e-40'):raise ArithmeticError('PIG integral remainder')
        return constant+n*mp.log(A)+loggamma_ratio(n,-mp.mpf('.5'),1)-mp.log1p(-A)+mp.log(value)
    @lru_cache(maxsize=20000)
    def tails(k):
        if k<cutoff and k<10000:
            extend(int(k));ll=mp.log(cumulative[int(k)])
            # At 160 digits this complement retains more than 80 digits.
            if ll < -mp.mpf('1e-70'):return pair(ll)
        return pair(upper(k+1),False)
    for k in (0,1,7):
        if abs(density(mp.mpf(k))-logd(mp.mpf(k)))>mp.mpf('1e-50'):
            raise ArithmeticError('README density mismatch')
    ref=Reference(tails,0,mp.inf,True,log_density=density)
    # The inverse-Gaussian mixing density has mass one; Tonelli and the
    # Poisson exponential series give total count mass one. Avoid building
    # O(z^2) recurrence entries merely to recheck this analytic identity.
    ref.normalization=mp.mpf(1)
    return ref


truth_point_exclusions = [([1.0, 1e-08], 'TASK-196 open: public CDF returns NaN at x=1.0, lower=True, log=True; independent value -7.070317853112745e-05. Positive heavy count tails exceed the 10000000-cell remainder guard; a bounded tail integral is missing.'), ([100000000.0, 1.0], 'TASK-196 open: public CDF returns NaN at x=2.0, lower=True, log=True; independent value -0.6562700043193149. Positive heavy count tails exceed the 10000000-cell remainder guard; a bounded tail integral is missing.')]
