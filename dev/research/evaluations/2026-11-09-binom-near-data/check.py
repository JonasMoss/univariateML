#!/usr/bin/env python3
"""Exact coefficients and 80-digit independent Jensen/remainder checks."""
from fractions import Fraction as F
from math import factorial, ceil
import json
import mpmath as mp
mp.mp.dps=80
mm=lambda v: mp.mpf(v.numerator)/v.denominator if isinstance(v,F) else mp.mpf(v)
def check(count,rate):
    U=len(count)-1;n=sum(count)
    p=[F(v,n) for v in count];weights=[F(rate)**k/factorial(k) for k in range(U+1)]
    q=[v/sum(weights) for v in weights]
    E=lambda f:sum(q[k]*f(k) for k in range(U+1))
    a=lambda k:-F(k*(k-1),2);b=lambda k:-F(k*(k-1)*(2*k-1),12);c=lambda k:-F(k*k*(k-1)*(k-1),12)
    mean=E(lambda k:k);aa=E(a);beta=(E(lambda k:k*a(k))-mean*aa)/(E(lambda k:k*k)-mean**2)
    h=lambda k:a(k)-aa-beta*(k-mean)
    delta=lambda f:sum((p[k]-q[k])*f(k) for k in range(U+1))
    assert delta(lambda k:k)==0 and delta(a)==0
    B=delta(b)-E(lambda k:h(k)**2)/2;assert B==0
    C=delta(c)-E(lambda k:h(k)*b(k)+h(k)**3/6)
    R=E(lambda k:F(2,3)*h(k)**4+abs(h(k)*c(k))+F(3,4)*sum(j**4 for j in range(k)))
    H=max(abs(h(k)) for k in range(U+1));end=ceil(max(2*U,4*H,4*R/-C)) if C<0 else None
    checks=0
    for N in sorted(set([ceil(max(2*U,2*H)),16,32,64,128,512,2048]+([end,2*end] if end else []))):
        if N<2*U or N<2*H:continue
        qt=[mm(q[k])*(1+mm(h(k))/N) for k in range(U+1)]
        cc=[sum(mp.log1p(-mp.mpf(j)/N) for j in range(k)) for k in range(U+1)]
        jensen=sum(qt[k]*mp.log(qt[k]/mm(q[k]))+(mm(p[k])-qt[k])*cc[k] for k in range(U+1))
        bound=mm(C)/N**3+mm(R)/N**4
        assert jensen<=bound+mp.mpf('1e-70')
        # Independently check the normalized degree-eight comparison.
        scaled=mp.mpf(0)
        for kk in range(U+1):
            hh=mm(h(kk));z=hh/N
            entropy=sum((-1)**r*hh**3*z**(r-3)/(r*(r-1)) for r in range(3,9))
            er=abs(hh)**3*abs(z)**6/(72*(1-abs(z)))
            lr=sum(mp.mpf(j)**3*(mp.mpf(j)/N)**6/(9*(1-mp.mpf(j)/N)) for j in range(kk))
            rest=-sum(mp.mpf(j)**3*(mp.mpf(j)/N)**(r-3)/r for j in range(kk) for r in range(3,9))
            coefficient=mm(p[kk]-q[kk])-mm(q[kk])*z
            scaled+=mm(q[kk])*(entropy+er-hh*mm(b(kk)))+coefficient*rest+abs(coefficient)*lr
        assert jensen*N**3<=scaled+mp.mpf('1e-65')
        if C<0 and N>=64:assert scaled<0
        for factor in ['.25','1','4']:
            lam=mm(rate)*mp.mpf(factor)
            w=[lam**k/mp.factorial(k)*mp.exp(cc[k]) for k in range(U+1)]
            diff=sum(mm(p[k])*(mp.log(w[k]/sum(w))-mp.log(mm(q[k]))) for k in range(U+1))
            assert diff<=jensen+mp.mpf('1e-70');checks+=1
    return dict(count=count,rate=str(rate),B=str(B),C=str(C),R=str(R),cutoff=end,uniform_checks=checks)
records=[check([242187,60672,121344,1488980,406274],4),check([21251,11790,0,244158,168771],6)]
# Each input is the exact binary64 value, not the real critical inflation.
near=[];S=10;k=5;n0=5;F2=16
# The alternating Taylor sums bracket exp(-8/5) exactly. At the unique ZIP
# face root, A=5*rate-8; its sign is the sign of pi-3*(1-pi)*exp(-8/5).
# Uniqueness follows because roots lie in [1,2] and score' <= -10/4+5/4<0.
qlo=sum((-F(8,5))**j/factorial(j) for j in range(64))
qhi=sum((-F(8,5))**j/factorial(j) for j in range(65))
assert qlo<qhi
for pi in [0.3772146069463776,0.37721460694637765]:
    inflation=mm(F(pi))
    w=lambda u:(1-inflation)*mp.exp(-u)/(inflation+(1-inflation)*mp.exp(-u))
    score=lambda u:S/u-k-n0*w(u)
    u=mp.findroot(score,(mp.mpf('1.59'),mp.mpf('1.61')))
    A=-F2/2+u*S-(k+n0*w(u))*u*u/2
    posterior=w(u)
    curvature=-S/u**2+n0*posterior*(1-posterior)
    slope=S-(k+n0*posterior)*u+n0*posterior*(1-posterior)*u**2/2
    fixed_second=S*u**2/2-(k+n0*posterior)*u**3/3-8+n0*posterior*(1-posterior)*u**4/8
    B=fixed_second-slope**2/(2*curvature)
    assert B<0
    record=dict(binary64=repr(pi),exact=str(F(pi)),rate=str(u),first=str(A),second=str(B))
    dlo=F(pi)-3*(1-F(pi))*qhi
    dhi=F(pi)-3*(1-F(pi))*qlo
    assert dhi<0 if A<0 else dlo>0
    record['exact_sign_certificate']=dict(difference_lower=str(dlo),difference_upper=str(dhi),
        sign=-1 if dhi<0 else 1,exponential_taylor_orders=[63,64])
    if A>0:
        record.update(formal_size_scale=str(-2*B/A),formal_likelihood_gain=str(-A**2/(4*B)))
    # Direct finite-size checks, not an integer search or a claim of global
    # optimality. Evaluate the full likelihood difference without lgamma
    # subtraction at huge N. This independently checks the tiny face signs.
    finite=[]
    for N in [10**4,10**8,10**12,10**16,4*10**16,10**17,10**18]:
        NN=mp.mpf(N)
        def finite_weight(v):
            q0=mp.exp(NN*mp.log1p(-v/NN))
            return (1-inflation)*q0/(inflation+(1-inflation)*q0)
        v=mp.findroot(lambda v:S/v-k-n0*finite_weight(v),(mp.mpf('1.59'),mp.mpf('1.61')))
        q0=mp.exp(NN*mp.log1p(-v/NN))
        correction=sum(mp.log1p(-mp.mpf(j)/NN) for x in [1,1,2,2,4] for j in range(x))
        difference=S*mp.log(v/u)+correction+(k*NN-S)*mp.log1p(-v/NN)+k*u
        difference+=n0*mp.log((inflation+(1-inflation)*q0)/(inflation+(1-inflation)*mp.exp(-u)))
        finite.append(dict(size=N,rate=str(v),prob=str(v/NN),loglik_minus_face=str(difference)))
    assert all(mp.mpf(v['loglik_minus_face'])<0 for v in finite[:4])
    if A>0:
        assert mp.mpf(finite[4]['loglik_minus_face'])>0
    else:
        assert all(mp.mpf(v['loglik_minus_face'])<0 for v in finite)
    record['finite_spot_checks']=finite
    near.append(record)
assert mp.mpf(near[0]['first'])<0<mp.mpf(near[1]['first'])
# At the real critical inflation the face rate is 8/5 and the posterior
# base-zero weight is 1/4. These Taylor coefficients are exact rationals.
u=F(8,5);w0=F(1,4)
curvature=-F(S)/u**2+n0*w0*(1-w0)
slope=n0*w0*(1-w0)*u**2/2
fixed_second=F(S)*u**2/2-(k+n0*w0)*u**3/3-8+n0*w0*(1-w0)*u**4/8
profile_second=fixed_second-slope**2/(2*curvature)
assert profile_second==F(-776,285)
formal=dict(face_curvature=str(curvature),first_derivative=str(slope),
            fixed_second=str(fixed_second),profile_second=str(profile_second))
print(json.dumps(dict(precision=80,third_order=records,near_critical=near,formal_critical=formal),indent=2))
# Independent finite profiles at the observed winning sizes and neighbours.
profiles=[]
for count,Ns,rate in [([242187,60672,121344,1488980,406274],range(4,25),4),([21251,11790,0,244158,168771],range(4,25),6)]:
    n=sum(count);mean=mp.mpf(sum(k*v for k,v in enumerate(count)))/n
    values=[]
    for N in Ns:
        coef=[mp.binomial(N,k) for k in range(5)]
        def law(eta):
            w=[coef[k]*mp.exp(k*eta) for k in range(5)];return w,sum(w)
        def score(eta):
            w,z=law(eta);return sum(k*w[k] for k in range(5))/z-mean
        left=mp.mpf(-30);right=mp.mpf(30)
        assert score(left)<0<score(right)
        for _ in range(300):
            mid=(left+right)/2
            if score(mid)<0:left=mid
            else:right=mid
        eta=(left+right)/2
        w,z=law(eta);prob=1/(1+mp.exp(-eta));ll=sum(count[k]*mp.log(w[k]/z) for k in range(5) if count[k])
        values.append(dict(size=N,prob=str(prob),logLik=str(ll)))
    best=max(values,key=lambda d:mp.mpf(d['logLik']))
    qw=[mp.mpf(rate)**k/mp.factorial(k) for k in range(5)]
    face=sum(count[k]*mp.log(qw[k]/sum(qw)) for k in range(5) if count[k])
    profiles.append(dict(count=count,face_rate=rate,face_logLik=str(face),finite_checks=values,best_in_checked_prefix=best))
# A separate JSON file keeps stdout's first evidence record valid JSON.
from pathlib import Path
Path(__file__).with_name('profiles.json').write_text(json.dumps(profiles,indent=2)+'\n')
