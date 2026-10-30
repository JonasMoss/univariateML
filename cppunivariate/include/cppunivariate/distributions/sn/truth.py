"""Positive Gaussian wedge integral, independently checked by density quadrature.

Polar coordinates in the two independent normals defining skew-normal give
F(-t) = 1/pi integral_0^atan(1/a) exp(-t^2/(2 sin(r)^2)) dr, t>=0,a>0.
No Owen-T routine is called. The independent tests integrate 2 phi Phi
on the real line, and check F(z)=Phi(z)^2 at a=1 and reflection.
The integrand's log has negative second derivative, so an endpoint tangent
bounds any truncated remainder by g(end)/|log(g)'(end)|. Require this bound
<1e-40 of the integral and quadrature error estimate <1e-38. Gaussian
concentration is scaled at the upper angular boundary; geometric splits
resolve the lower boundary layer when t is small. Near zero the density
bound 2/sqrt(2*pi) controls replacing F(-t) by F(0). At |z|>1e40 Mills
inequalities bound the thin-tail relative correction by (3+3/a^2)/z^2;
require it below 1e-35. The wide-tail correction is <=Phi(-a*z).
For z>0, S(z)=2 Phi(-z)-F(-z), subtracting at most half of 2 Phi(-z).
Normalization uses f(z)+f(-z)=2 phi(z). Arithmetic uses at least 80 digits.
"""
from functools import lru_cache
import mpmath as mp
from quality.truth_reference import Reference, pair, normal


def build(p, logd):
    xi,w,a=p['xi'],p['omega'],p['alpha']
    def logphi(t):
        return normal(t)[0] if abs(t)>1e5 else mp.log(mp.erfc(-t/mp.sqrt(2))/2)
    def ld(z,shape):return mp.log(2)-mp.log(2*mp.pi)/2-z*z/2+logphi(shape*z)
    def density(x):return ld((x-xi)/w,a)-mp.log(w)
    @lru_cache(maxsize=20000)
    def standard(z,shape):
        if shape<0:
            ll,lu=standard(-z,-shape);return lu,ll
        if shape==0:return normal(z)
        left=z<0;t=abs(z)
        if t>mp.mpf('1e40'):
            if left:
                bound=(3+3/(shape*shape))/(t*t)
                if bound>mp.mpf('1e-35'):raise ArithmeticError('thin Gaussian bound')
                lp=mp.log(2)-mp.log(2*mp.pi)-(1+shape*shape)*t*t/2-mp.log(shape*(1+shape*shape)*t*t)
            else:
                if normal(shape*t)[1]>mp.log(mp.mpf('1e-35')):
                    raise ArithmeticError('wide Gaussian bound')
                lp=mp.log(2)+normal(t)[1]
            return pair(lp,left)
        digits=80+2*max(0,int(mp.log10(max(1,t))))
        with mp.workdps(digits):
            theta=mp.atan(1/shape)
            if t==0:return pair(mp.log(theta/mp.pi))
            # Density <= 2/sqrt(2*pi): bound changes from the exact value at 0.
            if 2*t/mp.sqrt(2*mp.pi)<theta/mp.pi*mp.mpf('1e-48'):
                thin=mp.log(theta/mp.pi)
            else:
                scale=1/max(1,shape*(1+shape*shape)*t*t,t*mp.sqrt(1+shape*shape))
                full=theta/scale;end=min(mp.mpf(160),full)
                base=-(1+shape*shape)*t*t/2
                def integrand(v):
                    r=theta-scale*v
                    return mp.exp(-t*t/(2*mp.sin(r)**2)-base) if r>0 else mp.mpf(0)
                cuts=[mp.mpf(0),end]+[mp.mpf(v) for v in (1,4,16,64) if v<end]
                if end==full:
                    r=t
                    while r<theta:
                        cuts.append((theta-r)/scale);r*=4
                cuts=sorted(set(cuts))
                for degree in range(4,9):
                    value,error=mp.quad(integrand,cuts,method='gauss-legendre',maxdegree=degree,error=True)
                    if error<value*mp.mpf('1e-45'):break
                if end<full:
                    r=theta-scale*end
                    slope=scale*t*t*mp.cos(r)/mp.sin(r)**3
                    bound=integrand(end)/slope
                else:bound=mp.mpf(0)
                if error>value*mp.mpf('1e-38') or bound>value*mp.mpf('1e-40'):
                    raise ArithmeticError('Gaussian wedge quadrature/remainder bound')
                thin=base+mp.log(scale*value/mp.pi)
            if left:return pair(thin)
            wide=mp.log(2)+normal(t)[1]
            return pair(wide+mp.log1p(-mp.exp(thin-wide)),False)
    for z in (-1/(1+abs(a)),mp.mpf(0),1/(1+abs(a))):
        if abs(density(xi+w*z)-logd(xi+w*z))>mp.mpf('1e-50'):
            raise ArithmeticError('README density mismatch')
    ref=Reference(lambda x:standard((x-xi)/w,a),log_density=density)
    ref.numerical_tails=True
    ref.normalization_integrand=lambda z:mp.exp(ld(z,a))+mp.exp(ld(-z,a))
    ref.normalization_domain=[0,1,4,12,mp.inf]
    # Safeguarded Newton inversion uses this independently integrated CDF and
    # its analytic density; a final narrow bracket controls root error.
    roots=[]
    @lru_cache(maxsize=1000)
    def quantile(lp,lower):
        if a==0:
            # Exact normal member: avoid Newton residuals around its zero median.
            return xi+w*Reference(normal).quantile(lp,lower)
        if lp in (-mp.inf,0):
            return (-mp.inf if lower else mp.inf) if lp==-mp.inf else (mp.inf if lower else -mp.inf)
        index=0 if lower else 1
        def below(z):
            v=standard(z,a)[index]
            return v<lp if lower else v>lp
        lo,hi=mp.mpf(-1),mp.mpf(1)
        while not below(lo):lo*=2
        while below(hi):hi*=2
        same=[(abs(lp-oldlp),oldz) for oldlp,oldlower,oldz in roots if oldlower==lower]
        candidate=min(same,key=lambda item:item[0])[1] if same else (lo+hi)/2
        z=candidate if lo<candidate<hi else (lo+hi)/2
        for _ in range(160):
            value=standard(z,a)[index]
            if below(z):lo=z
            else:hi=z
            if abs(value-lp)<mp.mpf('1e-36'):
                delta=mp.mpf('1e-32')*max(1,abs(z))
                if below(z-delta) and not below(z+delta):
                    roots.append((lp,lower,z))
                    return xi+w*z
            slope=mp.exp(ld(z,a)-value)*(1 if lower else -1)
            candidate=z-(value-lp)/slope
            z=candidate if lo<candidate<hi else (lo+hi)/2
        raise ArithmeticError('Gaussian quantile convergence')
    ref.quantile=quantile
    return ref
