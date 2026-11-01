"""Skew-t tails by the positive Gaussian-mixture wedge integral.

This independent mpmath route uses the conditional-Gaussian representation;
normalization follows from f(z)+f(-z)=2 t_nu(z). It does not call C++.
"""
from functools import lru_cache
import mpmath as mp
from quality.truth_reference import Reference, student, pair

use_spec = True


def build(p, logd):
    xi, omega, alpha, nu = (p[k] for k in ('xi','omega','alpha','nu'))
    def ld(z):
        h = alpha*z*mp.sqrt((nu+1)/(nu+z*z))
        small = mp.betainc((nu+1)/2,mp.mpf('.5'),0,(nu+1)/(nu+1+h*h),regularized=True)/2
        cdf = small if h<0 else 1-small
        return (mp.log(2)-mp.log(omega)+mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)
                -mp.log(nu*mp.pi)/2-(nu+1)*mp.log1p(z*z/nu)/2+mp.log(cdf))
    @lru_cache(maxsize=10000)
    def standard(z,a):
        if a<0:
            l,u=standard(-z,-a)
            return u,l
        if a==0:return student(nu,z)
        t=abs(z)
        theta=mp.atan(1/a)
        if t==0:thin=mp.log(theta/mp.pi)
        else:
            base=-nu/2*mp.log1p(t*t/(nu*mp.sin(theta)**2))
            def f(r):
                if r<=0:return mp.mpf(0)
                return mp.exp(-nu/2*mp.log1p(t*t/(nu*mp.sin(r)**2))-base)
            value=mp.quad(f,[0,theta/16,theta/4,theta/2,theta])
            thin=base+mp.log(value/mp.pi)
        if z<=0:return pair(thin)
        ts=student(nu,t)[1]
        wide=mp.log(2)+ts
        return pair(wide+mp.log1p(-mp.exp(thin-wide)),False)
    def density(x):return ld((x-xi)/omega)
    for z in (-1,0,1):
        if abs(density(xi+omega*z)-logd(xi+omega*z))>mp.mpf('1e-40'):
            raise ArithmeticError('README density mismatch')
    ref=Reference(lambda x:standard((x-xi)/omega,alpha),log_density=density)
    ref.numerical_tails=True
    ref.normalization_integrand=lambda z:omega*(mp.exp(ld(z))+mp.exp(ld(-z)))
    ref.normalization_domain=[0,1,4,12,mp.inf]
    roots=[]
    @lru_cache(maxsize=1000)
    def quantile(lp,lower):
        if lp in (-mp.inf,0):
            return (-mp.inf if lower else mp.inf) if lp==-mp.inf else (mp.inf if lower else -mp.inf)
        if alpha==0 and lp==-mp.log(2):return xi
        if abs(alpha)==1 and lp==-mp.log(4) and ((alpha>0 and lower) or (alpha<0 and not lower)):return xi
        index=0 if lower else 1
        def below(t):
            value=standard(mp.sinh(t),alpha)[index]
            return value<lp if lower else value>lp
        lo,hi=mp.mpf(-1),mp.mpf(1)
        while not below(lo):lo*=2
        while below(hi):hi*=2
        same=[(abs(lp-v),t) for v,tail,t in roots if tail==lower]
        candidate=min(same,key=lambda x:x[0])[1] if same else (lo+hi)/2
        t=candidate if lo<candidate<hi else (lo+hi)/2
        for _ in range(200):
            z=mp.sinh(t); value=standard(z,alpha)[index]
            if below(t):lo=t
            else:hi=t
            if abs(value-lp)<mp.mpf('1e-36'):
                delta=mp.mpf('1e-32')*max(1,abs(t))
                if below(t-delta) and not below(t+delta):
                    roots.append((lp,lower,t))
                    return xi+omega*z
            slope=mp.exp(ld(z)+mp.log(omega)+mp.log(mp.cosh(t))-value)*(1 if lower else -1)
            candidate=t-(value-lp)/slope
            t=candidate if lo<candidate<hi else (lo+hi)/2
        raise ArithmeticError('skew-t quantile iteration did not converge')
    ref.quantile=quantile
    return ref
