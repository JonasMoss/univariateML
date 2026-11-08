"""Independent 70-digit pair roots and uniform remainder checks (no core calls)."""
import json, random
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
out=Path(__file__).parent
sample=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())['data']
from collections import Counter
d=Counter(sample)
def inc(z,c): return mp.loggamma(z+c)-mp.loggamma(z)
def ll(r,a,b):
    return sum(n*(inc(r,x)+inc(b,x)-mp.loggamma(x+1)+inc(a,r)-inc(a+b,r+x)) for x,n in d.items())
def score(r,a,b):
    sa=sum(n*(mp.digamma(a+r)+mp.digamma(a+b)-mp.digamma(a)-mp.digamma(a+b+r+x)) for x,n in d.items())
    sb=sum(n*(mp.digamma(a+b)-mp.digamma(b)+mp.digamma(b+x)-mp.digamma(a+b+r+x)) for x,n in d.items())
    return sa,sb
roots=[]
for r,start in [('1.0672234787479615',(2.14,5.36)),('5.3647338351087397',(2.14,1.067)),('1',(2,5)),('5',(2,1))]:
    r=mp.mpf(r)
    a,b=mp.findroot(lambda a,b:score(r,a,b),start,tol=mp.mpf('1e-60'))
    roots.append(dict(size=str(r),alpha=str(a),beta=str(b),loglik=str(ll(r,a,b)),score=[str(v) for v in score(r,a,b)]))
# Gamma-increment error bound and beta-mixture box bound sampled independently.
rng=random.Random(11642)
checks=0
for _ in range(200):
    r,a,b=[mp.mpf(10)**rng.uniform(-2,3) for _ in range(3)]
    x=rng.randint(0,120)
    p=a/(a+b);s=a+b
    actual=inc(r,x)-mp.loggamma(x+1)+inc(a,r)+inc(b,x)-inc(a+b,r+x)
    nb=inc(r,x)-mp.loggamma(x+1)+r*mp.log(p)+x*mp.log(1-p)
    rem=(r*(r+2)/p+x*(x+2)/(1-p)+(r+x)*(r+x+2))/(2*s)
    assert abs(actual-nb)<=rem
    # Poisson exterior inequality, uniform in alpha (no ratio restriction).
    lam=r*b/a
    po=-lam+x*mp.log(lam)-mp.loggamma(x+1)
    assert actual<=po+(lam**2+mp.mpf(x)*(x-1)/2)*(1/r+1/b)
    m=r*b/(a+r+b)
    assert actual<=-m+inc(m,x)-mp.loggamma(x+1)
    checks+=3
(out/'oracle.json').write_text(json.dumps(dict(dps=mp.mp.dps,roots=roots,uniform_inequality_checks=checks),indent=2)+'\n')
print(json.dumps(roots,indent=2))
print('Uniform inequality checks:',checks)
