#!/usr/bin/env python3
"""Independent 70-digit checks of remaining-profile-bounds.md (not a fitter)."""
import json
from pathlib import Path
import sys
import mpmath as mp
mp.mp.dps=70
v=list(map(mp.mpf,['.1','.5','1','2.4']))
n=len(v);g=sum(map(mp.log,v))/n;c=-g;vmin=min(v);delta=1-vmin
R=lambda z:-mp.log(-mp.expm1(-z))
r=lambda h:sum(R(h*z) for z in v)/n
qg=mp.exp(mp.findroot(lambda z:z-mp.digamma(mp.exp(z))-c,mp.log(1/(2*c))))
G=qg*mp.log(qg)-mp.loggamma(qg)-qg+(qg-1)*g
W=-mp.log(delta)-1;Q=4/c;C=mp.log(n)+mp.euler
H=mp.mpf(1)
while not (H>=1/vmin and H*delta>=mp.log(4*n) and H*delta-mp.log(n)-1>=2
           and 2/(H*delta-mp.log(n)-1)<mp.exp(-C-1)
           and mp.log(H)-C+1>0 and 2*mp.e*H*mp.exp(-H*vmin)<mp.log(H)-C+1):
    H*=2
rows=[]
for h in map(mp.mpf,['.001','.01','.1','1','10',str(H),str(2*H),str(4*H)]):
    rr=r(h)
    if h<=1:p0,q0=qg/h,qg
    else:p0=1/(h*delta);q0=max(mp.mpf('.01'),p0*n*mp.exp(h*vmin))
    def scores(lp,lq):
        p,q=mp.exp(lp),mp.exp(lq)
        return mp.digamma(p+q)-mp.digamma(p)-h,mp.digamma(p+q)-mp.digamma(q)-rr
    lp,lq=mp.findroot(scores,(mp.log(p0),mp.log(q0)),tol=mp.mpf('1e-55'),maxsteps=100)
    p,q=mp.exp(lp),mp.exp(lq)
    cp=h+mp.log(-mp.expm1(-rr));cq=rr+mp.log(-mp.expm1(-h))
    assert mp.exp(-rr)/h<=p<2/cp and mp.exp(-h)/rr<=q<2/cq
    L=mp.log(h)-p*h-(q-1)*rr-mp.log(mp.beta(p,q))
    row=dict(h=float(h),p=float(p),q=float(q),profile=float(L))
    if h<=c/2:
        assert q<=Q
        upper=G+max(1,Q)*h
        assert L<upper
        row['small_bound_slack']=float(upper-L)
    if h>=H:
        unsigned=W+2/(h*delta-mp.log(n)-1)*(mp.log(4*n+1)+mp.euler)+R(h*vmin)
        signed=W+R(h*vmin)+mp.exp(-1)/h*(C-1-mp.log(h))
        assert L<signed<W and L<unsigned
        row.update(unsigned_slack=float(unsigned-L),signed_slack=float(signed-L),power_face_gap=float(W-L))
    rows.append(row)
fixed=[]
for q in map(mp.mpf,['.3','2']):
    Gq=q*mp.log(q)-mp.loggamma(q)-q+(q-1)*g
    Aq=max(0,mp.digamma(q)-mp.digamma(1))
    for h in map(mp.mpf,['.001','.1',str(H),str(2*H)]):
        rr=r(h)
        p=mp.exp(mp.findroot(lambda z:mp.digamma(mp.exp(z)+q)-mp.digamma(mp.exp(z))-h,
                            mp.log(q/h if h<1 else 1/h),tol=mp.mpf('1e-55'),maxsteps=100))
        L=mp.log(h)-p*h-(q-1)*rr-mp.log(mp.beta(p,q))
        small=Gq+max(1,q)*h
        large=-1+Aq*(q+1)/h+max(0,1-q)*R(h*vmin)
        assert L<small and L<large
        entry=dict(q=float(q),h=float(h),profile=float(L))
        if q<1 and h>=H:
            Bq=mp.digamma(1)-mp.digamma(q);bq=mp.digamma(2)-mp.digamma(1+q)
            signed=-1-bq/(h+Bq)+(1-q)*R(h*vmin)
            assert 2*(1-q)*(h+Bq)*mp.exp(-h*vmin)<bq
            assert L<signed<-1
            entry['signed_slack']=float(signed-L)
        fixed.append(entry)
out=dict(evidence='70-digit checks of exact-arithmetic inequalities; bounds are not implemented in the remaining fitters',
         signed_large_cutoff=float(H),free_shapes=rows,held_q=fixed)
s=json.dumps(out,indent=2)+'\n'
if len(sys.argv)>1 and sys.argv[1]!='-':Path(sys.argv[1]).write_text(s)
else:print(s,end='')
