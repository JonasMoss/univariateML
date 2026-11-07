"""Independent 80-digit gamma/digamma roots and box-bound checks.

Run through the local CPU slot lock. JSON is written to stdout; no package installs.
These roots check coefficients; global optimality comes from the C++ cover proof.
"""
import json, random
from collections import Counter
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
D=Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom')
g=json.loads((D/'golden.json').read_text())

def ll(tab,a,r,b):
    return sum(c*(mp.loggamma(r+k)-mp.loggamma(r)-mp.loggamma(k+1)
        +mp.loggamma(a+r)+mp.loggamma(b+k)-mp.loggamma(a+r+b+k)
        -mp.loggamma(a)-mp.loggamma(b)+mp.loggamma(a+b)) for k,c in tab.items())
def score(tab,a,r,b):
    n=sum(tab.values())
    return (mp.digamma(a+r)-mp.digamma(r)+sum(c*(mp.digamma(r+k)-mp.digamma(a+r+b+k)) for k,c in tab.items())/n,
        mp.digamma(a+b)-mp.digamma(b)+sum(c*(mp.digamma(b+k)-mp.digamma(a+r+b+k)) for k,c in tab.items())/n)
records=[]
for name,x,a,guess in [('golden-alpha2',g['data'],mp.mpf(2),(mp.mpf('1.15'),mp.mpf('4.5'))),
    ('golden-held-golden-alpha',g['data'],mp.mpf('2.1419791546447171'),(mp.mpf('1.06'),mp.mpf('5.36'))),
    ('small-diagonal',[0,0,1,2,3,6,12,24],mp.mpf('.5'),(mp.mpf('1.1'),mp.mpf('1.1'))),
    ('small-offdiagonal',[0,0,1,2,3,6,12,24],mp.mpf(2),(mp.mpf('.75'),mp.mpf('10.4')))]:
    tab=Counter(x);n=len(x)
    r,b=mp.findroot(lambda r,b:score(tab,a,r,b),guess,tol=mp.mpf('1e-70'))
    if r>b:r,b=b,r
    value=ll(tab,a,r,b)
    ds=mp.findroot(lambda s:score(tab,a,s,s)[0],(mp.sqrt(r*b)*mp.mpf('.9'),mp.sqrt(r*b)*mp.mpf('1.1')))
    # Reproduce effective cutoffs independently and check all corner signs.
    lower=mp.mpf(1)
    small=lambda l:sum(c*min(0,mp.loggamma(l+k)-mp.loggamma(l)-mp.loggamma(k+1)) for k,c in tab.items() if k)
    while small(lower)>=value-mp.mpf('.00001'):lower/=2
    upper=mp.mpf(1)
    while score(tab,a,upper,lower)[0]>=-mp.mpf('1e-20'):upper*=2
    assert lower<r<upper and lower<b<upper
    # Sampled verification of the monotone likelihood-box upper bound against
    # separately evaluated gamma masses. This checks the bound, not coverage.
    rng=random.Random(11634)
    for _ in range(100):
        rl=mp.exp(rng.uniform(float(mp.log(lower)),float(mp.log(upper))))
        rh=rl*mp.exp(rng.uniform(0,.5))
        bl=mp.exp(rng.uniform(float(mp.log(lower)),float(mp.log(upper))))
        bh=bl*mp.exp(rng.uniform(0,.5))
        p0=mp.loggamma(a+rl)+mp.loggamma(a+bl)-mp.loggamma(a)-mp.loggamma(a+rl+bl)
        bound=n*p0+sum(c*(mp.loggamma(rh+k)-mp.loggamma(rh)+mp.loggamma(bh+k)-mp.loggamma(bh)
           -mp.loggamma(a+rh+bh+k)+mp.loggamma(a+rh+bh)-mp.loggamma(k+1)) for k,c in tab.items())
        assert ll(tab,a,(rl+rh)/2,(bl+bh)/2)<=bound+mp.mpf('1e-65')
    # Effective superlevel cutoff, independent of the smaller stationary
    # rectangle used by the fitter. Every observed mass is at most this CDF
    # whenever one shape >= V and the other >= lower (stochastic ordering).
    def cdf_bound(v):
        term=mp.exp(mp.loggamma(a+v)+mp.loggamma(a+lower)-mp.loggamma(a)-mp.loggamma(a+v+lower))
        total=term
        for k in range(max(tab)):
            term *= (v+k)/(a+v+lower+k)*(lower+k)/(k+1)
            total += term
        return len(x)*mp.log(total)
    exponent=0
    while cdf_bound(mp.mpf(2)**exponent)+mp.mpf('1e-20')>=value-mp.mpf('.00001'):
        exponent+=1
        assert exponent<200, 'research-only precision guard for these four fixtures'
    exterior=cdf_bound(mp.mpf(2)**exponent)
    records.append(dict(case=name,alpha=str(a),size=str(r),beta=str(b),logLik=str(value),
        diagonal_size=str(ds),diagonal_logLik=str(ll(tab,a,ds,ds)),
        stationary_rectangle=[str(lower),str(upper)],small_shape_loglik_upper=str(small(lower)),
        upper_size_score=str(score(tab,a,upper,lower)[0]),box_bound_checks=100,
        superlevel_rectangle=[str(lower),str(mp.mpf(2)**exponent)],
        superlevel_upper_power_of_two=exponent,
        large_shape_loglik_upper=str(exterior),exterior_comparison_margin='1e-20'))
print(json.dumps(dict(precision=mp.mp.dps,records=records),indent=2))
