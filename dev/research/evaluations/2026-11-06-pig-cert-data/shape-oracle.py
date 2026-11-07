"""180-digit independent finite-polynomial shape profiles and Poisson faces."""
from collections import Counter
import json
from pathlib import Path
from mpmath import mp
mp.dps = 180
root = Path(__file__).parent
cases = [
    ([(0,4),(10,1)],None), ([(0,4),(10,1)],'3'),
    ([(0,1),(1,1),(2,1),(3,1)],'.5'),
    ([(0,1),(1,1),(2,1),(3,1)],'3'),
    ([(1,1),(2,1)],'1e100'), ([(2,1),(5,1)],'.0001'),
    ([(10,1),(40,1)],None), ([(960,1),(1040,1)],None),
    ([(0,10002),(2,10000),(3,1)],None),
]
golden = json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/pig/golden.json').read_text())
cases.append((sorted(Counter(golden['data']).items()),None))
rows, requests = [], []
for counts, held in cases:
    n = sum(v for k,v in counts)
    avg = mp.mpf(sum(k*v for k,v in counts))/n
    mu = avg if held is None else mp.mpf(held)
    def polynomial(k,z):
        if k <= 1:
            return mp.mpf(1),mp.mpf(0)
        m=k-1
        term=mp.mpf(1)
        terms=[term]
        for j in range(1,k):
            term *= mp.mpf((m+j)*(m-j+1))/(2*j*z)
            terms.append(term)
        total=mp.fsum(terms)
        return total,mp.fsum(j*t for j,t in enumerate(terms))/total
    def score(s):
        lam=mp.exp(s)
        z=mp.sqrt(2*lam+(lam/mu)**2)
        q=lam/mu/z
        E=mp.fsum(v*polynomial(k,z)[1] for k,v in counts)/n
        return 2*mu*q/(1+q)**2-avg+(1+q*q)/(1-q*q)*E
    lo,hi=mp.mpf(-300),mp.mpf(300)
    assert score(lo)<0<score(hi)
    for _ in range(220):
        mid=(lo+hi)/2
        if score(mid)>0:
            hi=mid
        else:
            lo=mid
    lam=mp.exp((lo+hi)/2)
    z=mp.sqrt(2*lam+(lam/mu)**2)
    ll=mp.fsum(v*(lam/mu-z+k*mp.log(lam/z)+mp.log(polynomial(k,z)[0])-mp.loggamma(k+1)) for k,v in counts)
    if max(k for k,v in counts)<100:
        ll_bessel=mp.fsum(v*(mp.log(2*lam/mp.pi)/2+lam/mu+(mp.mpf(k)-mp.mpf('.5'))/2*mp.log(lam/(2+lam/mu**2))
                  +mp.log(mp.besselk(mp.mpf(k)-mp.mpf('.5'),z))-mp.loggamma(k+1)) for k,v in counts)
        assert abs(ll-ll_bessel)<mp.mpf('1e-150')
    rows.append(dict(counts=counts,held_mean=held,mean=str(mu),shape=str(lam),logLik=str(ll)))
    requests.append(' '.join([held or '0',str(len(counts))]+[str(x) for pair in counts for x in pair]))
(root/'shape-oracle.json').write_text(json.dumps(rows,indent=2)+'\n')
(root/'shape-oracle-input.txt').write_text('\n'.join(requests)+'\n')
print('PASS: 10 high-precision global shape profiles')
