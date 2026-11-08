#!/usr/bin/env python3
"""Independent numerical checks of the algebraic pair bound (not its proof)."""
import json, math, random
r=random.Random(11638)
worst=0.0
for j in range(20000):
    a=math.exp(r.uniform(0,12))
    u=math.exp(r.uniform(-15,8))/a
    # log(a*sinh(u)/cosh(a*u)), stable even far into the tails.
    log_sinh=u+math.log(-math.expm1(-2*u))-math.log(2)
    t=a*u
    log_cosh=t+math.log1p(math.exp(-2*t))-math.log(2)
    log_ratio=math.log(a)+log_sinh-log_cosh
    assert log_ratio < 1e-12, (a,u,log_ratio)
    p=math.exp(r.uniform(-12,-math.log(2)))
    log_rw=(math.log(a)+math.log(p)+math.log(-math.expm1(-2*u))
            -2*u*(a-1)*(1-p)-math.log(p+(1-p)*math.exp(-2*a*u)))
    log_rf=log_rw-2*u*(1-2*p)
    assert log_rw<=log_ratio+1e-10 and log_rf<=log_rw+1e-10
    worst=max(worst,math.exp(min(0,log_ratio)))

# Uniform standardized-density envelope and the two compact-box face bounds.
left_tail_checks=0
for j in range(3000):
    a=r.uniform(-1,1);L=r.uniform(-8,8)
    z=math.expm1(a*L)/a if a else L
    h=math.exp(-(1+a)*L-math.exp(-L))
    assert h<=1+1e-12 and z*z*h<=1+1e-12
    if z<=-1:
        assert h<=(1-z)**2*math.exp(z-1)+1e-12
        left_tail_checks+=1
    T=10**r.uniform(-2,2);eta=10**r.uniform(-9,-1)
    v=T*r.random();t=(1-eta)*v;p=1/(1-eta)
    C=max(math.exp(-1),T*T*math.log(max(1,T)))
    D=2*math.log(max(1,T))+2*C+T
    excess=(p-1)*math.log(t)-t**p+v
    assert excess<=D*eta+1e-12
    Z=10**r.uniform(-1,.6);xi=r.uniform(-1,1)*min(1,.4/Z)
    zz=Z*r.uniform(-1,1);rr=abs(xi)*Z
    LL=math.log1p(xi*zz)/xi if xi else zz
    diff=-(1+xi)*LL-math.exp(-LL)+zz+math.exp(-zz)
    bound=abs(xi)*(Z/(1-rr)+Z*Z/(2*(1-rr))*(1+math.exp(Z/(1-rr))))
    assert abs(diff)<=bound+1e-12
print(json.dumps(dict(seed=11638,pair_checks=20000,weighted_pair_checks=20000,max_pair_ratio=worst,envelope_checks=3000,reverse_face_checks=3000,gumbel_face_checks=3000,left_tail_checks=left_tail_checks,margin=1e-12),indent=2))
