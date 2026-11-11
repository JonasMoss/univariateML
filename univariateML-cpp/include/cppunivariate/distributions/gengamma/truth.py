"""Independent high-precision generalized-gamma antiderivative.

Small shapes use positive standardized-coordinate integration; larger shapes
use mpmath's incomplete gamma. No production code is called.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, normal, gamma

special_points = {'Q': [-.01, .01, -1e-8, 1e-8, -2., 2., -10.]}
boundary_parameters = {'Q': [10., -1e-6, 1e-6, -1e-4, 1e-4]}


def build(t):
    mu, sigma, q = t['mu'], t['sigma'], t['Q']
    if not q:
        return Reference(lambda x: normal((mp.log(x)-mu)/sigma), 0, mp.inf)
    a=1/q**2
    if abs(q) >= mp.mpf('.05'):
        def tails(x):
            w=(mp.log(x)-mu)/sigma
            v=gamma(a,a*mp.exp(q*w))
            return v if q>0 else (v[1],v[0])
        return Reference(tails,0,mp.inf)

    rem=mp.loggamma(a)-(a-mp.mpf('.5'))*mp.log(a)+a-mp.log(2*mp.pi)/2
    def penalty(w):
        return (mp.expm1(q*w)-q*w)/q**2
    def logdw(w):
        return -mp.log(2*mp.pi)/2-rem-penalty(w)
    def logsmall(w):
        direction=-1 if w<0 else 1
        slope=mp.expm1(q*w)/q
        rate=max(mp.mpf(1),abs(slope))
        z0=q*w
        def integrand(y):
            d=direction*y/rate
            diff=d*slope+mp.exp(z0)*(mp.expm1(q*d)-q*d)/q**2
            return mp.exp(-diff) if diff < 1200 else mp.mpf(0)
        value=mp.quad(integrand,[0,1,4,16,64,256,1024])
        return logdw(w)+mp.log(value/rate)
    def tails(x):
        w=(mp.log(x)-mu)/sigma
        return pair(logsmall(w),w<0)
    class SmallReference(Reference):
        def quantile(self,lp,lower):
            if lp == -mp.inf: return self.lower if lower else self.upper
            if lp == 0: return self.upper if lower else self.lower
            if lp > -mp.log(2):
                lp=mp.log(-mp.expm1(lp)); lower=not lower
            sign=-1 if lower else 1
            w=sign*mp.sqrt(max(mp.mpf('.01'),-2*lp))
            lo,hi=mp.mpf(-1),mp.mpf(1)
            def residual(v):
                r=tails(mp.exp(mu+sigma*v))[0 if lower else 1]-lp
                return r if lower else -r
            while residual(lo)>0: lo=2*lo-1
            while residual(hi)<0: hi=2*hi+1
            w=min(hi,max(lo,w))
            for _ in range(100):
                r=residual(w)
                if abs(r) < mp.mpf('1e-45')*max(1,abs(lp)):
                    return mp.exp(mu+sigma*w)
                if r<0: lo=w
                else: hi=w
                if hi-lo < mp.mpf('1e-45')*max(1,abs(w)): break
                derivative=mp.exp(logdw(w)-tails(mp.exp(mu+sigma*w))[0 if lower else 1])
                proposed=w-r/derivative
                w=proposed if lo<proposed<hi else (lo+hi)/2
            return mp.exp(mu+sigma*(lo+hi)/2)
    ref=SmallReference(tails,0,mp.inf)
    ref.numerical_tails=True
    ref.normalization_integrand=lambda w: mp.exp(logdw(w))
    ref.normalization_domain=[-80,-20,0,20,80]
    return ref

# Joint/finite-design limit stresses beyond the historical grid.
extreme_parameter_sets = [{'Q': 1e-08}, {'Q': -1e-08}]
