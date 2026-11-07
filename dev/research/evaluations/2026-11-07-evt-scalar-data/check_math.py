"""Independent finite-difference and envelope checks, seed 11630; no fitter calls."""
import math, random, json
r=random.Random(11630)
worst=0.; checks=0
for case in range(300):
    n=r.randrange(3,20);a=math.exp(r.uniform(-1,2));g=[0.]+[math.exp(r.uniform(-4,4)) for _ in range(n-1)]
    if a*(n-1)<=1: continue
    def f(u):
        ds=[v+math.exp(u) for v in g]
        S=sum(d**(-a) for d in ds)
        return n*math.log(n*a)-n-n*math.log(S)-(a+1)*sum(map(math.log,ds))
    def score(u):
        rs=[math.exp(u)/(v+math.exp(u)) for v in g]
        return n*a*sum(v**(a+1) for v in rs)/sum(v**a for v in rs)-(a+1)*sum(rs)
    u=r.uniform(-5,5);h=1e-4
    num=(f(u+h)-f(u-h))/(2*h);exact=score(u)
    err=abs(num-exact)/n;worst=max(worst,err);assert err<1e-7
    C=n*math.log(n*a)-n-(a+1)*sum(math.log(v) for v in g if v>0)
    small=C+(a*(n-1)-1)*u
    large=n*math.log(a)-n-n*u+n*a*math.log1p(max(g)*math.exp(-u))
    assert f(u)<=min(small,large)+1e-10*n
    checks+=3
for case in range(300):
    x=[r.uniform(-3,3) for _ in range(8)];s=math.exp(r.uniform(-1,1));xi=r.uniform(-.99,-.01)
    mu=max(x)+s/xi+math.exp(r.uniform(-1,1))
    def f(m):
        ts=[1+xi*(v-m)/s for v in x]
        return -len(x)*math.log(s)-sum((1+1/xi)*math.log(t)+t**(-1/xi) for t in ts)
    ts=[1+xi*(v-mu)/s for v in x]
    score=sum((1+xi-t**(-1/xi))/t for t in ts)/s
    curvature=(1+xi)*sum((xi-t**(-1/xi))/t**2 for t in ts)/s**2
    h=1e-5
    assert curvature<0
    assert abs((f(mu+h)-f(mu-h))/(2*h)-score)<1e-5*(1+abs(score))
    checks+=2
print(json.dumps(dict(seed=11630,checks=checks,worst_frechet_score_error_per_observation=worst,status='pass'),indent=2))
