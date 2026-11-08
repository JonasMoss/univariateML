#!/usr/bin/env python3
"""Numerically check the report's proved held-p tail/chord inequalities.

This is an independent formula check, not a certificate for the C++ mesh.
Run under a CPU slot. Optional argv[1] is the JSON output path.
"""
import json
from pathlib import Path
import sys
import mpmath as mp
mp.mp.dps=70
v=list(map(mp.mpf,['.1','.5','1','2.4']))
n=len(v)
g=sum(map(mp.log,v))/n
vmin=min(v)
def r(a):
    return -sum(mp.log(-mp.expm1(-a*y)) for y in v)/n
def dr(a):
    return -sum(y/mp.expm1(a*y) for y in v)/n
def qstar(p,z):
    assert z>0
    lo=mp.mpf(0);hi=max(1,2*p/z)
    for _ in range(300):
        q=(lo+hi)/2
        score=mp.digamma(q+p)-mp.digamma(q)-z
        if score>0:lo=q
        else:hi=q
        if lo>0 and hi-lo<mp.mpf('1e-55')*lo:break
    else:raise AssertionError('q root')
    q=(lo+hi)/2
    return q
def conjugate(p,z):
    q=qstar(p,z)
    return -q*z-mp.log(mp.beta(p,q))
def profile(p,a):
    z=r(a)
    return mp.log(a)-p*a+z+conjugate(p,z)
checks=[]
for p in map(mp.mpf,['.1','.5','1','2','10']):
    small=mp.mpf('.001')
    rlower=-mp.log(small)-g
    u1=-g+max(0,1-p)*small+mp.log(max(1,p))-mp.log(rlower)-1
    u2=mp.log(small)-mp.loggamma(p)+p*mp.log(p+1)
    A=mp.log(p)+mp.euler if p>1 else mp.mpf(0)
    sharp=-g+max(0,1-p)*small-mp.log(rlower-A)-1
    upper_small=max(sharp,u2)
    large=max(mp.log(2)/vmin,1/(p*(1-vmin)))*2
    upper_large=mp.log(large)-p*(1-vmin)*large+p*mp.log(n)+1+p*mp.log(p)-mp.loggamma(p)
    margins=[]
    for f in ['1','.5','.01','1e-10']:
        margins.append(upper_small-profile(p,small*mp.mpf(f)))
    for f in ['1','2','4']:
        margins.append(upper_large-profile(p,large*mp.mpf(f)))
    for l,h in [('0.1','0.2'),('1','1.1'),('10','10.1')]:
        l,h=map(mp.mpf,(l,h));rl,rh=r(l),r(h);dl,dh=dr(l),dr(h)
        k=(rh-rl+dl*l-dh*h)/(dl-dh)
        rk=rl+dl*(k-l)
        assert l<k<h and rk>0
        mid=(l+h)/2
        bound=max(mp.log(mid)+(a-mid)/mid-p*a+z+conjugate(p,z)
                  for a,z in [(l,rl),(h,rh),(k,rk)])
        def K(a):return sum(w*w*mp.exp(-a*w)/(-mp.expm1(-a*w))**2 for w in v)/n
        def J(q):return mp.polygamma(1,q)-mp.polygamma(1,q+p)
        ql,qh=qstar(p,rl),qstar(p,rh)
        dl,dh=-dr(l),-dr(h)
        sl=1/h-p+(ql-1)*(dh if ql>=1 else dl)
        sh=1/l-p+(qh-1)*(dl if qh>=1 else dh)
        ch=-1/h**2+(-(ql-1)*K(h) if ql>=1 else (1-ql)*K(l))+dl**2/J(qh)
        cl=-1/l**2+(-(qh-1)*K(l) if qh>=1 else (1-qh)*K(h))+dh**2/J(ql)
        for i in range(11):
            a=l+(h-l)*i/10
            q=qstar(p,r(a));d=-dr(a)
            score=1/a-p+(q-1)*d
            curvature=-1/a**2-(q-1)*K(a)+d**2/J(q)
            margins.extend([bound-profile(p,a),score-sl,sh-score,curvature-cl,ch-curvature])
    assert min(margins)>0
    checks.append(dict(p=float(p),minimum_bound_slack=float(min(margins)),
                       small_cutoff=float(small),small_upper=float(upper_small),
                       large_cutoff=float(large),large_upper=float(upper_large)))
out=dict(evidence='numerical checks of exact-arithmetic inequalities',cases=checks)
if len(sys.argv)>1 and sys.argv[1]!='-':Path(sys.argv[1]).write_text(json.dumps(out,indent=2)+'\n')
else:print(json.dumps(out,indent=2))
