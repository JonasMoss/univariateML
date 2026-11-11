#!/usr/bin/env python3
"""80-digit numerical checks of new uniform upper inequalities, not a proof."""
import json
import mpmath as mp
mp.mp.dps=80
checks=0
H=mp.mpf(1)/8
for variance in (False,True):
    for q in map(mp.mpf,('0','.01','.5','1','4','30','1000')):
        A=(q*q-(6 if variance else 2)*q+(3 if variance else -1))/4
        R=(mp.mpf('.5')+mp.mpf('.75')*q*q if variance else q*q/4)+H/24
        for h in map(mp.mpf,('.125','.01','.001','1e-8','1e-12')):
            v=2+1/h if variance else 1/h
            r=v-2 if variance else v
            g=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v/2)/2
            difference=g+mp.log(v/r)/2-(v+1)*mp.log1p(q/r)/2+q/2
            assert difference <= A*h+R*h*h
            checks+=1
        h=mp.mpf('1e-22');v=2+1/h if variance else 1/h;r=v-2 if variance else v
        g=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v/2)/2
        actual=(g+mp.log(v/r)/2-(v+1)*mp.log1p(q/r)/2+q/2)/h
        assert abs(actual-A)<mp.mpf('1e-12')*(1+abs(A))
        checks+=1
# Check the optimized-scale quadratic envelope on normalized held-centre data.
for original in ((-2,-1,-.3,.2,.8,1.4,2.1),(-1.5,-1,-.5,.5,1,1.5)):
    x=list(map(mp.mpf,original));n=len(x);s0=mp.sqrt(mp.fsum(z*z for z in x)/n)
    y=[z/s0 for z in x];q=[z*z for z in y];S4=mp.fsum(z*z for z in q)
    A=(S4-3*n)/4
    assert A<0
    rho=mp.mpf('.05');curvature=2*n*mp.exp(-2*rho)
    M=max(abs(n*mp.exp(2*rho)-S4*mp.exp(-4*rho)),abs(n*mp.exp(-2*rho)-S4*mp.exp(4*rho)))
    K=S4*mp.exp(4*rho)/4+n*H/24+M*M/(2*curvature)
    for h in (min(H,-A/(2*K))/8,min(H,-A/(2*K))/32):
        v=1/h;lo=mp.mpf('.1')*min(abs(z) for z in y);hi=mp.mpf(2)
        for _ in range(240):
            mid=(lo+hi)/2
            score=mp.fsum((z/mid**2-1)/(1+z*h/mid**2) for z in q)
            if score>0: lo=mid
            else: hi=mid
        s=(lo+hi)/2;t=mp.log(s)
        assert abs(t)<rho
        g=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v/2)/2
        delta=n*g-n*t-(v+1)*mp.fsum(mp.log1p(z*h/(s*s)) for z in q)/2+n/2
        assert delta<=A*h+K*h*h<0
        checks+=1
counterexamples=[]
for variance in (False,True):
    x=([mp.mpf(0)]*26+[-mp.mpf(1),mp.mpf(1)]*7 if variance else
       [-mp.mpf('.001'),mp.mpf('.001')]*3+[-mp.mpf(1),mp.mpf(1)]*2)
    n=len(x);sN=mp.sqrt(mp.fsum(z*z for z in x)/n)
    q=[(z/sN)**2 for z in x]
    A=(mp.fsum(z*z for z in q)-3*n)/4
    v=mp.mpf(3) if variance else mp.mpf('.2')
    physical=sN if variance else mp.mpf('.001')
    ordinary=physical*mp.sqrt((v-2)/v) if variance else physical
    ll=n*(mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2-mp.log(ordinary))-(v+1)*mp.fsum(mp.log1p((z/ordinary)**2/v) for z in x)/2
    normal=-n*(mp.log(2*mp.pi)/2+mp.log(sN)+mp.mpf('.5'))
    assert A<0 and ll>normal
    counterexamples.append(dict(family='std' if variance else 'lst',coefficient=str(A),finite_witness_gain=str(ll-normal)))
    checks+=1
for v in map(mp.mpf,('.1','.5','1','2','2.00000001','3','8','32','100','1e6')):
    cp=(mp.digamma((v+1)/2)-mp.digamma(v/2)-1/v)/2
    assert mp.mpf('.25')-1/(8*v*v)<=v*v*cp<=mp.mpf('.25')
    checks+=1
    if v>2:
        a=(v-2)/v;f=a*a/4-a;actual=(v-2)**2*cp-a
        assert f-a*a/(8*v*v)<=actual<=f
        checks+=1
# Positive-series R(u) used by scalar and profile score enclosures.
for u in map(mp.mpf,('0','1e-22','1e-12','.0001','.01','.011','.05','.1','.125')):
    t=u/(1+u)
    value=mp.fsum(t**k/k for k in range(2,19))
    tail=t**19/(19*(1-t))
    truth=mp.log1p(u)-u/(1+u)
    assert value<=truth+mp.mpf('1e-79') and truth<=value+tail+mp.mpf('1e-79')
    checks+=1
print(json.dumps(dict(precision_dps=80,checks=checks,failed=0,counterexamples=counterexamples,scope='Numerical verification; uniform proof is in signed-derivation.md'),indent=2))
