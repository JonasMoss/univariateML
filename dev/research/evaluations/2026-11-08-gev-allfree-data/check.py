"""Independent high-precision two-point reduction, no fitted-library calls."""
import mpmath as mp
import json, random
mp.mp.dps=70
rng=random.Random(11651)
def face(p):
    q=1-p
    def score(t): return 1/t-q+q/(p*mp.exp(t)+q)
    lo,hi=1/q,2/q
    while score(hi)>0: hi*=2
    for _ in range(260):
        mid=(lo+hi)/2
        if score(mid)>0: lo=mid
        else: hi=mid
    t=(lo+hi)/2
    return t,mp.log(t)-q*t-mp.log(p+q*mp.exp(-t))-1

def finite(p):
    q=1-p
    h=mp.sqrt(p*q)-p
    s=1-2*p
    return h,s,mp.log(s)-2*p*mp.log(h)-2*q*mp.log1p(h)-1
checks=0
rows=[]
for k in range(1,50):
    p=mp.mpf(k)/100;q=1-p
    t,L0=face(p);h,s,L1=finite(p)
    assert abs(s-h*(1+h)/(p+h))<mp.mpf('1e-65')
    for _ in range(20):
        b=mp.mpf(str(rng.uniform(.0001,.9999)))
        u=mp.exp(mp.mpf(str(rng.uniform(-5,5))))
        f=lambda b: mp.log(mp.expm1(b*u)/b)-q*(1+b)*u-mp.log(p+q*mp.exp(-u))-1
        assert f(b)<max(mp.log(u)-q*u-mp.log(p+q*mp.exp(-u))-1,f(1))
        assert f(b)<max(L0,L1)
        checks+=1
    if k in (5,20,40,45,49):
        rows.append(dict(p=str(p),h=str(h),sigma=str(s),gumbel_t=str(t),finite_loglik_per_n=str(L1),gumbel_loglik_per_n=str(L0)))
threshold=mp.findroot(lambda p:finite(p)[2]-face(p)[1],(.4,.42))
print(json.dumps(dict(precision=mp.mp.dps,seed=11651,strict_convexity_checks=checks,crossing=str(threshold),references=rows),indent=2))
