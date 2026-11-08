#!/usr/bin/env python3
"""Independent exact/70-digit checks of a mean-preserving second-order bound."""
import json, math
from fractions import Fraction as F
import mpmath as mp
mp.mp.dps=70

def moments(P,Q,rate):
    n=sum(P);Z=sum(Q);U=len(Q)-1
    p=[F(v,n) for v in P];q=[F(v,Z) for v in Q]
    a=[-F(k*(k-1),2) for k in range(U+1)]
    b=[-F(k*(k-1)*(2*k-1),12) for k in range(U+1)]
    mean=sum(q[k]*k for k in range(U+1))
    abar=sum(q[k]*a[k] for k in range(U+1))
    assert sum(p[k]*k for k in range(U+1))==mean
    assert sum(p[k]*a[k] for k in range(U+1))==abar
    var=sum(q[k]*(k-mean)**2 for k in range(U+1))
    cov=sum(q[k]*(k-mean)*(a[k]-abar) for k in range(U+1))
    beta=cov/var
    h=[a[k]-abar-beta*(k-mean) for k in range(U+1)]
    assert sum(q[k]*h[k] for k in range(U+1))==0
    assert sum(q[k]*k*h[k] for k in range(U+1))==0
    B=sum((p[k]-q[k])*b[k] for k in range(U+1))-sum(q[k]*h[k]**2 for k in range(U+1))/2
    R=F(2,3)*sum(q[k]*abs(h[k])**3 for k in range(U+1))+sum(q[k]*abs(h[k]*b[k]) for k in range(U+1))+sum(q[k]*a[k]**2 for k in range(U+1))
    H=max(map(abs,h))
    end=math.ceil(max(2*U,4*H,4*R/(-B))) if B<0 else None
    return n,Z,U,p,q,a,b,mean,h,B,R,H,end

def check(P,Q,rate,prefix=False):
    n,Z,U,p,q,a,b,mean,h,B,R,H,end=moments(P,Q,rate)
    mm=lambda x:mp.mpf(x.numerator)/x.denominator if isinstance(x,F) else mp.mpf(x)
    pm=list(map(mm,p));qm=list(map(mm,q));hm=list(map(mm,h))
    face=sum(pm[k]*mp.log(qm[k]) for k in range(U+1))
    checks=0;slack=mp.inf
    for N in sorted(set([int(mp.ceil(max(2*U,2*mm(H)))),16,32,64,128]+([end] if end else []))):
        if N<2*U or N<2*mm(H):continue
        qt=[qm[k]*(1+hm[k]/N) for k in range(U+1)]
        assert min(qt)>0 and abs(sum(qt)-1)<mp.mpf('1e-60')
        assert abs(sum(k*qt[k] for k in range(U+1))-mm(mean))<mp.mpf('1e-60')
        c=[sum(mp.log1p(-mp.mpf(j)/N) for j in range(k)) for k in range(U+1)]
        jensen=sum(qt[k]*mp.log(qt[k]/qm[k]) for k in range(U+1))+sum((pm[k]-qt[k])*c[k] for k in range(U+1))
        bound=mm(B)/N**2+mm(R)/N**3
        assert jensen<=bound+mp.mpf('1e-60')
        if end and N>=end:assert bound<0
        for factor in map(mp.mpf,['.3','1','3']):
            r=mm(rate)*factor
            w=[r**k/mp.factorial(k)*mp.exp(c[k]) for k in range(U+1)]
            ll=sum(pm[k]*(mp.log(w[k])-mp.log(sum(w))) for k in range(U+1))
            assert ll-face<=jensen+mp.mpf('1e-60')
            slack=min(slack,bound-(ll-face));checks+=1
    prefix_max=None
    if prefix and end:
        diffs=[]
        for N in range(max(k for k,v in enumerate(P) if v),end):
            c=[sum(mp.log1p(-mp.mpf(j)/N) for j in range(k)) for k in range(U+1)]
            def law(eta):
                w=[mp.exp(k*eta+c[k])/mp.factorial(k) for k in range(U+1)]
                return w,sum(w)
            lo=mp.log(mm(rate))-20;hi=mp.log(mm(rate))+20
            for _ in range(250):
                mid=(lo+hi)/2;w,z=law(mid)
                if sum(k*w[k] for k in range(U+1))/z<mm(mean):lo=mid
                else:hi=mid
            w,z=law((lo+hi)/2)
            assert abs(sum(k*w[k] for k in range(U+1))/z-mm(mean))<mp.mpf('1e-60')
            diff=n*(sum(pm[k]*(mp.log(w[k])-mp.log(z)) for k in range(U+1))-face)
            assert diff<0;diffs.append(diff)
        prefix_max=str(max(diffs));prefix_count=len(diffs)
    else:prefix_count=0
    return dict(P=P,Q=Q,rate=rate,B_exact=str(B),R_exact=str(R),H_exact=str(H),safe_cutoff=end,uniform_likelihood_checks=checks,minimum_slack=str(slack),prefix_checks=prefix_count,largest_prefix_excess=prefix_max)

records=[check([50,41,33,3,3],[48,48,24,8,2],1,True),check([1,9,6,10],[2,6,9,9],3),check([1,2,2],[1,2,2],2)]
positive=[]
for rate in [4,5,6,8,10,12,20,30]:
    Q=[24,24*rate,12*rate**2,4*rate**3,rate**4]
    P=[24+12*rate+2*rate**2,0,0,24*rate+16*rate**2+4*rate**3,rate**4-6*rate**2-12*rate]
    result=moments(P,Q,rate)
    positive.append(dict(rate=rate,P=P,Q=Q,B_exact=str(result[9]),B=float(result[9])))
assert records[0]['B_exact']=='-20333/128830'
assert any(r['B']>0 for r in positive)
print(json.dumps(dict(precision=70,proof='Mean-preserving Q_N=Q(1+h/N); B/N^2+R/N^3 upper bound uniform in probability.',records=records,positive_second_order_examples=positive),indent=2))
