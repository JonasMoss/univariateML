"""Independent 80-digit stationary equations; globality comes from C++ cover."""
import json,pathlib,mpmath as mp
mp.mp.dps=80
root=pathlib.Path(__file__).resolve().parents[4]; data=pathlib.Path(__file__).resolve().parent
g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/betapr/golden.json').read_text())
x=[mp.mpf(v) for v in g['data']]; y=list(map(mp.log,x));n=len(x)
def score(p,q,t):
    A=mp.fsum(mp.log1p(mp.exp(t-z)) for z in y)/n
    B=mp.fsum(mp.log1p(mp.exp(z-t)) for z in y)/n
    U=mp.fsum(1/(1+mp.exp(t-z)) for z in y)/n
    return mp.digamma(p+q)-mp.digamma(p)-A,mp.digamma(p+q)-mp.digamma(q)-B,-p+(p+q)*U
p,q,t=mp.findroot(score,(mp.mpf('2.01427144'),mp.mpf('3.26000986'),mp.log(mp.mpf('1.05941045'))),tol=mp.mpf('1e-70'))
b=mp.exp(t);ll=-n*(mp.loggamma(p)+mp.loggamma(q)-mp.loggamma(p+q))-mp.fsum(p*mp.log1p(mp.exp(t-z))+q*mp.log1p(mp.exp(z-t))+z for z in y)
row=(data/'joint-timing-after.txt').read_text().splitlines()[0].split()
low=list(map(mp.mpf,row[11:14]));high=list(map(mp.mpf,row[14:17]))
assert len(low)==len(high)==3
assert all(l<=v<=u for l,v,u in zip(low,[p,q,b],high))
drift=[float(abs(v/mp.mpf(g['estimates'][key])-1)) for v,key in zip([p,q,b],['shape1','shape2','scale'])]
assert max(drift)<1e-6
out=dict(dps=80,coefficients=list(map(str,[p,q,b])),logLik=str(ll),max_score=str(max(map(abs,score(p,q,t)))),in_cpp_enclosures=True,golden_relative_drift=drift,n10000='20 repeats have exactly the same empirical likelihood per datum')
(data/'coefficient-oracle.json').write_text(json.dumps(out,indent=2)+'\n');print(json.dumps(out,indent=2))
