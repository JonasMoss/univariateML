#!/usr/bin/env python3
import json
from fractions import Fraction as F
import mpmath as mp
mp.mp.dps=70
N=1075;n=1001;k=1000;S=535000
pi=mp.power(2,-1074)
a=n*k;b=-F(S*(n+k))-F(S,N);c=S*S+S
disc=b*b-4*a*c
assert disc<0
lo=mp.mpf(S)/(n*N);hi=mp.mpf(S)/(k*N)
def score(p):
    l0=N*mp.log1p(-p);lp=mp.log(pi);lq=mp.log1p(-pi)+l0
    scale=max(lp,lq);u=N*p
    return mp.exp(lp-scale)*(S-k*u)+mp.exp(lq-scale)*(S-n*u)
assert score(lo)>0 and score(hi)<0
for _ in range(250):
    mid=(lo+hi)/2
    if score(mid)>0:lo=mid
    else:hi=mid
p=(lo+hi)/2
zero=mp.log(pi+(1-pi)*(1-p)**N)
ll=zero+k*(mp.log1p(-pi)+mp.loggamma(N+1)-mp.loggamma(536)-mp.loggamma(N-534)+535*mp.log(p)+(N-535)*mp.log1p(-p))
fixed_zero=mp.log(pi+(1-pi)*mp.power(2,-1075))
fixed_ll=fixed_zero+mp.loggamma(N+1)-mp.loggamma(501)-mp.loggamma(N-499)-N*mp.log(2)
print(json.dumps(dict(precision=70,fixed_loglik=str(fixed_ll),tiny_zero_logmass=str(mp.log1p(-mp.mpf("5e-301"))),pi=str(pi),fixed_zero_logmass=str(fixed_zero),unique_probability_root=str(p),root_score=str(score(p)),profile_loglik=str(ll),turning_discriminant_exact=str(disc)),indent=2))
