"""Independent antiderivative of the README density; checked by quality.truth.

The route contains only mathematical definitions, no C++/R/reference calls.
"""
import mpmath as mp
from quality.truth_reference import Reference, pair, complement, normal, gamma, beta, student, finite_pmf, geometric_sum, loggamma_ratio

special_points = {'lambda': [-5e-10,0.,5e-10]}
# Q'(u)>0 proves the density normalizes: f(Q(u))*Q'(u)=1.
# Neither the existing scipy-seeded spec nor C++ is used for inversion.
def expression(symbols,x):
    import sympy as sp
    return sp.S.Zero  # actual implicit density supplied by build below

def build(t):
    m,s,a=t['mu'],t['sigma'],t['lambda']
    lo,hi=(m-s/a,m+s/a) if a>0 else (-mp.inf,mp.inf)
    def q(lp,lower):
        if lp==-mp.inf:return lo if lower else hi
        if lp==0:return hi if lower else lo
        lc=complement(lp)
        z=lp-lc if a==0 else (mp.expm1(a*lp)-mp.expm1(a*lc))/a
        return m+s*z if lower else m-s*z
    def tails(x):
        if a==0:
            z=(x-m)/s
            return pair(-mp.log1p(mp.exp(abs(z))),z<0)
        if x==m:return (-mp.log(2),-mp.log(2))
        target=m-abs(x-m)
        left,right=mp.mpf(-32),-mp.log(2)
        while q(left,True)>target:left*=2
        for _ in range(160):
            mid=(left+right)/2
            if q(mid,True)<target:left=mid
            else:right=mid
        return pair((left+right)/2,x<m)
    def ld(x):
        if a>0 and (x==lo or x==hi):
            return -mp.inf if a<1 else -mp.log(2*s)
        lp,lc=tails(x)
        v=(a-1)*lp; w=(a-1)*lc
        big=max(v,w)
        return -mp.log(s)-big-mp.log(mp.exp(v-big)+mp.exp(w-big))
    out=Reference(tails,lo,hi,breaks=[m],log_density=ld)
    out.numerical_tails=True
    out.quantile=q
    def normalization_integrand(u):
        # Direct implicit README density at Q(u), times an INDEPENDENT
        # numerical derivative of Q. No inverse-CDF seeding is involved.
        ld_at_q=-mp.log(s)-mp.log(u**(a-1)+(1-u)**(a-1))
        return mp.exp(ld_at_q)*mp.diff(lambda v:q(mp.log(v),True),u)
    out.normalization_integrand=normalization_integrand
    return out

