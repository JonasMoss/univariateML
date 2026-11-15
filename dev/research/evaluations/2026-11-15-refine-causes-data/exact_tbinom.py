"""tbinom route (cases 7325 free, 7353 a held): exact rational evaluation of the truncated-binomial likelihood.
ll(N, p; window [lo,hi]) = sum_i log( w(x_i) / sum_{k in window} w(k) ),  w(k) = C(N,k) p^k q^(N-k),  with p a double (exact rational).
Ratios w(k)/w(k0) are exact Fractions, so every value below is the log of an exact rational number."""
import math, json
from fractions import Fraction as F
import mpmath as mp
mp.mp.dps=50
x=[1000000,1000001,1000003,1000008,1000011]
def ll(N,p,lo,hi):
    p=F(p); r=p/(1-p); k0=lo
    # w(k)/w(k0) = C(N,k)/C(N,k0) * r^(k-k0); C(N,k+1)/C(N,k) = (N-k)/(k+1)
    ratio={}; cur=F(1); ratio[k0]=cur
    for k in range(k0,min(hi,N)):
        cur=cur*F(N-k,k+1)*r; ratio[k+1]=cur
    Z=sum(ratio.values())
    return sum(mp.log(mp.mpf(ratio[v].numerator)/ratio[v].denominator) for v in x)-len(x)*mp.log(mp.mpf(Z.numerator)/Z.denominator)
if __name__=='__main__':
    p_free=float.fromhex('0x1.ffff20c43cbfp-1'); N_free=1000011
    p_held=float.fromhex('0x1.fffc35a371662p-1'); N_held=1000035
    out={}
    lo,hi=1000000,1000011   # window of the free fit: [min, max]
    out['free_fit_returned']=dict(N=N_free,p=p_free,ll_exact=str(ll(N_free,p_free,lo,hi)),record=-float.fromhex('0x1.16d10539281dcp+4'))
    out['held_fit_point_in_free_window']=dict(N=N_held,p=p_held,ll_exact=str(ll(N_held,p_held,lo,hi)))
    # profile over N in the free window (p solved in mp by matching the window mean to the sample mean)
    def prof(N):
        mp.mp.dps=40
        xb=mp.mpf(sum(x))/len(x)
        def logw(k,eta): return mp.loggamma(N+1)-mp.loggamma(k+1)-mp.loggamma(N-k+1)+eta*k
        def mean(eta):
            L=[logw(k,eta) for k in range(lo,hi+1)]; m=max(L); w=[mp.exp(l-m) for l in L]; Z=sum(w)
            return sum(k*wi for k,wi in zip(range(lo,hi+1),w))/Z
        eta=mp.findroot(lambda e: mean(e)-xb, mp.log(mp.mpf(xb)/(N-xb)))
        L=[logw(k,eta) for k in range(lo,hi+1)]; m=max(L); lz=m+mp.log(sum(mp.exp(l-m) for l in L))
        return sum(logw(v,eta) for v in x)-len(x)*lz, 1/(1+mp.exp(-eta))
    out['profile']={}
    for N in [1000011,1000012,1000020,1000035,1000100,1000139,2000000,10**8,10**12]:
        v,pp=prof(N); out['profile'][str(N)]=[str(v),str(pp)]
    print(json.dumps(out,indent=1))
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/exact_tbinom.json','w'),indent=1)
