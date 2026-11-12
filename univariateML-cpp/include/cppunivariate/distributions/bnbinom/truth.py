"""Beta-mixture tail, accelerated by swapping the two beta integrals.

For n=k+1, S(k)=E[I_Y(a,b)], Y~Beta(r,n). Expanding I gives
 B(r+a,n)/(a B(a,b) B(r,n)) *
 sum_j (a)_j(1-b)_j(r+a)_j / ((a+1)_j(n+r+a)_j j!).
After j>=b-1, the absolute term ratio is <=(j+r+a)/(j+n+r+a).
The resulting gamma-ratio majorant telescopes: the remainder after j is
<= |term[j+1]|*(j+n+r+a)/(n-1), n>1. This controls even power-law tails.
Small n uses a finite lower sum; normalization follows from the beta/NB
mixture and is also checked by summing the body plus the controlled tail.

The inherited far-count gamma-ratio approximation has absolute log error
<=2*|u-v|*(max(|u|,|v|)+1)/x for x>2*max(|u|,|v|). Integrate
log(y)-1/y < digamma(y) < log(y) between the shifts to obtain this bound.
The helper's threshold makes it <2e-50 per ratio.
"""
from functools import lru_cache
import mpmath as mp
from quality.truth_reference import Reference, pair, loggamma_ratio


def build(p, logd):
    r,a,b = p['size'],p['alpha'],p['beta']
    c=mp.loggamma(a+r)+mp.loggamma(a+b)-mp.loggamma(r)-mp.loggamma(a)-mp.loggamma(b)
    def density(k):
        return c+loggamma_ratio(k,r,1)+loggamma_ratio(k,b,a+r+b)
    for k in (0,1,7):
        if abs(density(mp.mpf(k))-logd(mp.mpf(k)))>mp.mpf('1e-50'):
            raise ArithmeticError('README density mismatch')
    weights=[mp.exp(logd(mp.mpf(k))) for k in range(301)]
    sums=[]; total=mp.mpf(0)
    for w in weights:
        total+=w; sums.append(total)
    near_poisson=a>mp.mpf('1e12') and r*b/(a-1)<10
    @lru_cache(maxsize=20000)
    def moment_bound(n):
        # E[(X)_n]=(r)^rising_n*(b)^rising_n/(a-1)^falling_n.
        # Markov gives P(X>=n)<=E[(X)_n]/n! when n<a.
        if n>=a:raise ArithmeticError('BNB factorial-moment order unavailable')
        return (mp.loggamma(r+n)-mp.loggamma(r)+mp.loggamma(b+n)-mp.loggamma(b)
                +mp.loggamma(a-n)-mp.loggamma(a)-mp.loggamma(n+1))
    @lru_cache(maxsize=20000)
    def near_tail(n):
        if n>mp.mpf('1e50')*(a+r+b+1)**2:
            # Each gamma ratio has O((a+r+b)^2/n) log error. The
            # positive power sum lies between its integral and integral+first.
            return c-mp.log(a)-a*mp.log(n)
        first=density(n);term=mp.mpf(1);total=term
        for j in range(10000):
            end=n+j+1
            if moment_bound(end)<first+mp.log(total)+mp.log(mp.mpf('1e-48')):
                return first+mp.log(total)
            k=n+j
            term*=(r+k)*(b+k)/((k+1)*(a+r+b+k));total+=term
        raise ArithmeticError('BNB positive sum/factorial-moment remainder')
    @lru_cache(maxsize=20000)
    def tails(k):
        n=k+1
        if near_poisson:
            if n<=300 and mp.log(sums[int(k)]) < -mp.mpf('1e-50'):
                return pair(mp.log(sums[int(k)]))
            return pair(near_tail(n),False)
        if b==1 or r==1:
            rr = r if b==1 else b
            upper=mp.loggamma(rr+a)-mp.loggamma(rr)+loggamma_ratio(n,rr,rr+a)
            return pair(upper,False)
        if n<=300:
            return pair(mp.log(sums[int(k)]))
        term=mp.mpf(1); total=term
        for j in range(10000):
            nxt=term*(a+j)*(1-b+j)*(r+a+j)/((a+1+j)*(n+r+a+j)*(j+1))
            if nxt==0 or (j>=b-1 and abs(nxt)*(j+n+r+a)/(n-1)<abs(total)*mp.mpf('1e-48')):
                break
            total+=nxt;term=nxt
        else: raise ArithmeticError('beta-mixture remainder')
        pref=loggamma_ratio(n,r,r+a)+mp.loggamma(r+a)-mp.loggamma(r)-mp.log(a)-mp.log(mp.beta(a,b))
        return pair(pref+mp.log(total),False)
    ref=Reference(tails,0,mp.inf,True,log_density=density)
    ref.normalization=sums[300]+mp.exp(tails(mp.mpf(300))[1])
    return ref

# Joint limit paths supplement the shared one-coordinate extreme grid.
extreme_parameter_sets = [{'alpha': 100000000.0, 'beta': 100000000.0}, {'size': 100000000.0, 'beta': 100000000.0, 'alpha': 1e+16}]
