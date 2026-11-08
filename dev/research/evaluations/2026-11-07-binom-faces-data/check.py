#!/usr/bin/env python3
"""Independent 70-digit checks of the signed Jensen/compression inequalities."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
out=Path(__file__).resolve().parent
records=[]
for x,L,U in [([1,1,1,2,3,7,12],1,None),([2,3,4],2,4),([0,0,1,1,7],0,None),([3,3,4,8],3,8)]:
    n=len(x);mean=mp.mpf(sum(x))/n
    def weights(lam):
        if U is None:
            z=mp.exp(lam)-sum(lam**k/mp.factorial(k) for k in range(L))
            return z
        return sum(lam**k/mp.factorial(k) for k in range(L,U+1))
    def mu(lam):return lam*mp.diff(weights,lam)/weights(lam)
    lo=mp.mpf('1e-60');hi=mp.mpf(max(x)*2)
    for _ in range(250):
        mid=(lo+hi)/2
        if mu(mid)<mean:lo=mid
        else:hi=mid
    lam=(lo+hi)/2;Z=weights(lam)
    q=lambda k:lam**k/mp.factorial(k)/Z
    f2=lam**2*mp.diff(weights,lam,2)/Z
    f3=lam**3*mp.diff(weights,lam,3)/Z
    gap=sum(mp.mpf(v)*(v-1) for v in x)/n-f2
    M=f3/3+f2/2
    face=sum(v*mp.log(lam)-mp.loggamma(v+1)-mp.log(Z) for v in x)
    checked=0;max_error=-mp.inf
    for N in [max(x)+1,32,64,128]:
        K=min(U,N) if U is not None else N//2
        if K<=L or K>=N:continue
        qq={k:q(k) for k in range(L,K+1)}
        delta=mp.mpf(0)
        if U is None:
            tail=1-sum(qq.values())
            tailmean=mean-sum(k*v for k,v in qq.items())
            d=(tailmean-K*tail)/(K-L)
            if d>=qq[L]:continue
            qq[L]-=d;qq[K]+=tail+d
            delta=sum(v*mp.log(v/q(k)) for k,v in qq.items() if v>0)
            r=lam/(K+1)
            if r<1:
                H=r/(1-r)+r/((K-L)*(1-r)**2)
                assert delta<=q(K)*(1+H)*mp.log1p(H)+mp.mpf('1e-60')
            assert abs(sum(qq.values())-1)<mp.mpf('1e-60')
            assert abs(sum(k*v for k,v in qq.items())-mean)<mp.mpf('1e-60')
            assert sum(k*(k-1)*v for k,v in qq.items())<=f2+mp.mpf('1e-60')
        def c(k):return sum(mp.log1p(-mp.mpf(j)/N) for j in range(k))
        upper=delta-gap/(2*N)+M/(2*N*N*(1-mp.mpf(K-1)/N))
        for factor in [mp.mpf('.3'),mp.mpf('1'),mp.mpf('3')]:
            rate=lam*factor
            z=sum(rate**k/mp.factorial(k)*mp.exp(c(k)) for k in range(L,min(N,U) +1 if U is not None else N+1))
            ll=sum(v*mp.log(rate)-mp.loggamma(v+1)+c(v)-mp.log(z) for v in x)
            err=(ll-face)/n-upper
            assert err<mp.mpf('1e-55'),(x,N,err)
            max_error=max(max_error,err);checked+=1
    records.append(dict(data=x,L=L,U=U,lambda_=str(lam),gap=str(gap),checks=checked,max_positive_error=str(max_error)))
# Check the two monotone envelopes used to turn one cutoff into an infinite tail.
envelopes=0
for L in [0,1,3]:
    for lam in map(mp.mpf,['.3','2','20']):
        K0=int(mp.ceil(max(L+2,2*lam+2)))
        Z=mp.exp(lam)-sum(lam**k/mp.factorial(k) for k in range(L))
        prev_delta=prev_d=mp.inf
        for K in range(K0,K0+20):
            q=lam**K/mp.factorial(K)/Z;r=lam/(K+1)
            H=r/(1-r)+r/((K-L)*(1-r)**2)
            deltaN=q*(1+H)*mp.log1p(H)*(2*K+1)
            d=q*r/((K-L)*(1-r)**2)
            assert deltaN<prev_delta and d<prev_d
            prev_delta,prev_d=deltaN,d;envelopes+=1
# Normalized binomial/ZIP expansion, uniform in inflation and rate on the
# necessary stationary interval. This check does not optimize either density.
zip_checks=0;zip_min_slack=mp.inf
for x in [[0]*8+[1,1,1,2,3,7,12],[0,0,1,1,1,2,2,8],[0,1,2,3,4]]:
    n=len(x);positive=[v for v in x if v];k=len(positive);n0=n-k
    S=mp.mpf(sum(x));F2=sum(v*(v-1) for v in x);pm=S/k;m=S/n
    G=F2-S*pm;C=S*pm**2+n0*pm**4/32
    for N in [int(mp.ceil(max(max(x)+1,2*pm))),64,256]:
        for j in range(7):
            u=m+(pm-m)*j/6
            for pi in map(mp.mpf,['0','.01','.2','.8','.999']):
                def logmass(v,finite):
                    if finite:
                        f=mp.binomial(N,v)*(u/N)**v*(1-u/N)**(N-v)
                    else:f=mp.exp(-u)*u**v/mp.factorial(v)
                    return mp.log(pi+(1-pi)*f) if v==0 else mp.log1p(-pi)+mp.log(f)
                difference=sum(logmass(v,True)-logmass(v,False) for v in x)
                bound=-G/(2*N)+C/N**2
                assert difference<=bound+mp.mpf('1e-55')
                zip_min_slack=min(zip_min_slack,bound-difference);zip_checks+=1
# Independently check the localized held-inflation coefficient and likelihood
# envelopes (positive-count tangent plus zero-term chord).
box_checks=0;box_min_slack=mp.inf
for x in [[0]*5+[1,1,2,2,4],[0]*8+[1,1,1,2,3,7,12]]:
    n=len(x);positive=[v for v in x if v];k=len(positive);n0=n-k
    S=mp.mpf(sum(x));F2=sum(v*(v-1) for v in x);pm=S/k;m=S/n
    for pi in map(mp.mpf,['.2','.5','.9']):
        zero=lambda u:mp.log(pi+(1-pi)*mp.exp(-u))
        w=lambda u:(1-pi)*mp.exp(-u)/(pi+(1-pi)*mp.exp(-u))
        constant=k*mp.log1p(-pi)-sum(mp.loggamma(v+1) for v in positive)
        for j in range(8):
            a=m+(pm-m)*j/8;b=m+(pm-m)*(j+1)/8;t=(a+b)/2
            A=-F2/2+b*S-k*b*b/2-n0*a*a*w(b)/2
            base=S*mp.log(t)-k*t+constant;slope=S/t-k
            upper=max(base+slope*(a-t)+n0*zero(a),base+slope*(b-t)+n0*zero(b))
            for h in range(7):
                u=a+(b-a)*h/6
                exactA=-F2/2+u*S-(k+n0*w(u))*u*u/2
                exactF=S*mp.log(u)-k*u+constant+n0*zero(u)
                assert exactA<=A+mp.mpf('1e-60')
                assert exactF<=upper+mp.mpf('1e-60')
                box_min_slack=min(box_min_slack,A-exactA,upper-exactF);box_checks+=1
# Free-inflation profile envelopes and the inactive-inflation threshold.
free_boxes=inactive_checks=0
for x in [[0,1,1,1,2,4],[0,1,1,1,2,3,4,4]]:
    n=len(x);k=sum(v>0 for v in x);n0=n-k;S=mp.mpf(sum(x))
    m=S/n;v=S/k;z=mp.mpf(n0)/n;threshold=-mp.log(z)
    factorial=sum(mp.loggamma(v+1) for v in x)
    for j in range(8):
        a=m+(v-m)*j/8;b=m+(v-m)*(j+1)/8
        lo=mp.log(a);hi=mp.log(b);eta=(lo+hi)/2;mid=mp.exp(eta)
        grad=S-k*mid/(-mp.expm1(-mid))
        zero=-n0*b+k*mp.log(-mp.expm1(-b)) if b<threshold else n0*mp.log(z)+k*mp.log(mp.mpf(k)/n)
        upper=S*eta-k*(mid+mp.log(-mp.expm1(-mid)))-factorial+abs(grad)*(hi-lo)/2+zero
        for h in range(7):
            u=a+(b-a)*h/6;qhat=max(z,mp.exp(-u))
            exact=S*mp.log(u)-k*(u+mp.log(-mp.expm1(-u)))-factorial+n0*mp.log(qhat)+k*mp.log1p(-qhat)
            assert exact<=upper+mp.mpf('1e-60');free_boxes+=1
            if b<threshold:
                start=int(mp.ceil(max(max(x)+1,2*v,2*b*b/(threshold-b))))
                for N in [start,2*start,8*start]:
                    assert (1-u/N)**N>z;inactive_checks+=1

# Exact moment equality without empirical saturation on a finite window.
from fractions import Fraction
reference=[2,6,9,9];observed=[1,9,6,10]
for order in range(3):
    assert sum(Fraction(k**order*c,26) for k,c in enumerate(reference)) == sum(Fraction(k**order*c,26) for k,c in enumerate(observed))
assert Fraction(observed[1],observed[0]) != Fraction(2*observed[2],observed[1])
# The real-valued held inflation makes the local ZIP coefficient exactly zero;
# the C++/R fixture uses its nearest double and deliberately remains unresolved.
u=mp.mpf('1.6');q=mp.exp(-u);pi=3*q/(1+3*q)
w=(1-pi)*q/(pi+(1-pi)*q)
assert abs(u*(5+5*w)-10)<mp.mpf('1e-65')
assert abs(-8+u*10-(5+5*w)*u*u/2)<mp.mpf('1e-65')
# Factorial-moment Newton certificate, including the nonsaturated zero gap.
P=[1,9,6,10];Q=[2,6,9,9]
from math import comb
for r in range(2,4):
    assert sum(Fraction(comb(k,r)*c,26) for k,c in enumerate(P) if k>=r)>=sum(Fraction(comb(k,r)*c,26) for k,c in enumerate(Q) if k>=r)
newton_checks=0
for N in [3,4,8,16,128]:
    c=[sum(mp.log1p(-mp.mpf(j)/N) for j in range(k)) for k in range(4)]
    difference=sum((mp.mpf(P[k])-Q[k])*c[k] for k in range(4))/26
    formula=(mp.log1p(-mp.mpf(2)/N)-2*mp.log1p(-mp.mpf(1)/N))/26
    assert abs(difference-formula)<mp.mpf('1e-60') and difference<0
    newton_checks+=1
# Same two moments but mixed higher factorial moments do not meet that criterion.
P=[50,41,33,3,3];Q=[48,48,24,8,2]
for r in range(3):
    assert sum(Fraction(k**r*c,130) for k,c in enumerate(P))==sum(Fraction(k**r*c,130) for k,c in enumerate(Q))
assert sum(comb(k,3)*(P[k]-Q[k]) for k in range(3,5))==-1
assert P[4]-Q[4]==1
# Independent held-ZIP rate peaks at the exact binary64 inflation used by
# C++/R/JS. Their tiny nonzero value gap is not asserted to be an exact tie.
pi=mp.mpf(float('.7254971052291191'))
w=lambda u:(1-pi)*mp.exp(-u)/(pi+(1-pi)*mp.exp(-u))
score=lambda u:20/u-2-198*w(u)
rates=[mp.findroot(score,(mp.mpf('.5'),mp.mpf('.6'))),mp.findroot(score,(mp.mpf('9.9'),mp.mpf('9.99')))]
values=[];curvatures=[]
for rate in rates:
    assert abs(score(rate))<mp.mpf('1e-60')
    curvature=-20/rate**2+198*w(rate)*(1-w(rate))
    assert curvature<0
    value=198*mp.log(pi+(1-pi)*mp.exp(-rate))+2*mp.log1p(-pi)-2*rate+20*mp.log(rate)-mp.loggamma(20)
    values.append(value);curvatures.append(curvature)
assert abs(values[0]-values[1])<mp.mpf('1e-8')
finite_value=99*mp.log(pi)+mp.log1p(-pi)
face_original=[99*mp.log(pi+(1-pi)*mp.exp(-rate))+mp.log1p(-pi)-rate+10*mp.log(rate)-mp.loggamma(11) for rate in rates]
assert finite_value-max(face_original)>1
zip_tie_evidence=dict(pi_binary64=str(pi),rates=list(map(str,rates)),curvatures=list(map(str,curvatures)),value_gap=str(values[0]-values[1]),finite_incumbent_gap=str(finite_value-max(face_original)))
# Two-point windows are saturated for every feasible size: an exact tie family.
for N in [3,4,100]:
    # On {2,3}, successive kernel ratio is (N-2)*p/(3*(1-p)).
    p=mp.mpf(3)/(N+1)
    ratio=(N-2)*p/(3*(1-p))
    assert abs(ratio-1)<mp.mpf('1e-65')
(out/'checks.json').write_text(json.dumps(dict(precision=mp.mp.dps,records=records,two_point_ties=True,monotone_envelope_checks=envelopes,zip_uniform_checks=zip_checks,zip_minimum_slack=str(zip_min_slack),localized_box_checks=box_checks,localized_minimum_slack=str(box_min_slack),exact_zero_gap_moments=True,exact_stationary_zip_coefficient_zero=True,exact_newton_checks=newton_checks,mixed_higher_moment_fixture=True,zip_tied_face=zip_tie_evidence,free_profile_box_checks=free_boxes,inactive_inflation_checks=inactive_checks),indent=2)+'\n')
print('signed Jensen and mean-preserving compression checks passed;',sum(r['checks'] for r in records),'likelihood inequalities')
