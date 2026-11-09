"""Independent finite integer profiles, with an explicit Poisson tail bound.

For size r and maximum observation M<r, put a=1-M/r and lambda=r*p.
The binomial pmf is <= lambda**x exp(-a*lambda)/x!. After
conditioning positive and setting k=a*lambda, its likelihood is bounded
by the positive-Poisson likelihood plus sum(x)*log(1/a), since
1-exp(-k/a)>=1-exp(-k). Mixture zero probabilities are bounded by their
saturated empirical maximum. Thus the bound covers every r>=R, including
the infinite-size face. Enumeration below R uses high precision, not the
library fitter. Exhausting the requested cell budget raises an error.
"""
from collections import Counter
import mpmath as mp


def integer_profile(id, golden, max_cells=100000):
    held = set(golden.get('fixed', [])) | {k for k,v in golden.get('fit_args', {}).items() if v is not None}
    if held & {'prob', 'pi', 'p0'}:
        raise ValueError('integer profile with held probability requires a separate tail envelope')
    xs = golden['data']
    counts = Counter(xs)
    positive = {x:n for x,n in counts.items() if x>0}
    n, np = len(xs), sum(positive.values())
    M = int(max(xs))
    S = mp.mpf(sum(x*n for x,n in counts.items()))
    m = S/n
    tiny = mp.mpf('1e-55')
    def bisect(fn, lo, hi):
        for _ in range(240):
            mid=(lo+hi)/2
            if fn(mid)>0: hi=mid
            else: lo=mid
            if hi-lo<tiny*max(1,abs(mid)): return (lo+hi)/2
        raise ValueError('integer profile root budget exhausted')
    def zt_mean(lam):
        return lam/(-mp.expm1(-lam))
    mpz=S/np
    lam=bisect(lambda l:zt_mean(l)-mpz,tiny,mpz+1) if mpz>1 else mp.mpf(0)
    fact=mp.fsum(n*mp.loggamma(x+1) for x,n in positive.items())
    face=(S*mp.log(lam)-np*lam-np*mp.log(-mp.expm1(-lam))-fact) if lam else -fact
    saturated=mp.mpf(0)
    if id in ('zib','zmbinom'):
        if n>np: saturated+=(n-np)*mp.log(mp.mpf(n-np)/n)
        saturated+=np*mp.log(mp.mpf(np)/n)
        face+=saturated
    if id=='binom':
        face=S*mp.log(m)-n*m-fact
    def profile(r):
        if id=='binom':
            p=m/r
        else:
            def condmean(p):
                return r*p/(-mp.expm1(r*mp.log1p(-p)))
            p=bisect(lambda p:condmean(p)-mpz,tiny,1-tiny)
            if id=='zib' and -mp.expm1(r*mp.log1p(-p)) < mp.mpf(np)/n:
                p=m/r  # constrained mixture maximum is the ordinary binomial
        q=mp.exp(r*mp.log1p(-p))
        ll=mp.fsum(c*(mp.loggamma(r+1)-mp.loggamma(x+1)-mp.loggamma(r-x+1)
                    +x*mp.log(p)+(r-x)*mp.log1p(-p)) for x,c in positive.items())
        estimates={'size':mp.mpf(r),'prob':p}
        if id=='binom':
            ll+=(n-np)*r*mp.log1p(-p)
        elif id=='ztbinom': ll-=np*mp.log1p(-q)
        else:
            pi=(mp.mpf(n-np)/n-q)/(1-q) if id=='zib' else mp.mpf(n-np)/n
            if id=='zib' and pi<0: pi=mp.mpf(0)
            estimates['pi' if id=='zib' else 'p0']=pi
            if id=='zib':
                ll+=np*mp.log1p(-pi)
                if n>np: ll+=(n-np)*mp.log(pi+(1-pi)*q)
            else:
                ll-=np*mp.log1p(-q)
                ll+=saturated
        return ll,estimates
    seed=int(golden['estimates']['size'])
    best,estimates=profile(seed)
    if best<=face:
        raise ValueError('integer winner does not beat the Poisson tail envelope')
    R=max(M+1,seed+1)
    while face-S*mp.log1p(-mp.mpf(M)/R)>=best:
        R*=2
        if R-M>max_cells:
            raise ValueError(f'integer enumeration budget exhausted: tail needs {R-M} cells')
    second=mp.ninf
    for r in range(M,R):
        ll,t=profile(r)
        if ll>best:
            second,best,estimates=best,ll,t
        elif r!=int(estimates['size']): second=max(second,ll)
    return estimates,{'coefficient':'size','range':[M,R-1],'feasible_cells':R-M,
        'tail_upper_bound':mp.nstr(face-S*mp.log1p(-mp.mpf(M)/R),25),
        'tail_gap':mp.nstr(best-(face-S*mp.log1p(-mp.mpf(M)/R)),25),
        'runner_up_gap':mp.nstr(best-second,25),
        'argument':'binomial-to-Poisson envelope; see reference_integer.py'}
