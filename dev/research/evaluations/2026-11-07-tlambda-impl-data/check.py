#!/usr/bin/env python3
"""Independent 100-digit checks of compiled C++ witnesses; no optimizer oracle."""
import json, math, pathlib, subprocess, sys
import mpmath as mp
mp.mp.dps=100
rows=subprocess.check_output([sys.argv[1]],text=True).splitlines()
def exact(s): return mp.mpf(float(s))
def h(t,l):
    p=mp.exp(-t); q=-mp.expm1(-t)
    if l==0:return mp.log(q)-mp.log(p)
    return (mp.expm1(l*mp.log(q))-mp.expm1(l*mp.log(p)))/l
def ld(x,m,s,l):
    z=abs((x-m)/s)
    if l>0 and z>=1/l-mp.mpf('1e-90'):
        alias=l>=1 and (float(x)==float(m)-float(s)/float(l) or float(x)==float(m)+float(s)/float(l))
        if l>=1 and (alias or abs(z-1/l)<mp.mpf('1e-90')):
            return (-mp.log(s)-(mp.log(2) if l==1 else 0),mp.nan,mp.nan)
        return (-mp.inf,mp.nan,mp.nan)
    if z==0:p=mp.mpf('.5')
    else:
        a=mp.log(2);b=mp.mpf(2)
        while h(b,l)<z:b*=2
        for _ in range(360):
            t=(a+b)/2
            if h(t,l)<z:a=t
            else:b=t
        p=mp.exp(-(a+b)/2)
    u=p if x<=m else 1-p
    d=u**(l-1)+(1-u)**(l-1)
    dp=(l-1)*(u**(l-2)-(1-u)**(l-2))
    return (-mp.log(s)-mp.log(d),dp/(s*d*d),-1+(x-m)/s*dp/(d*d))
count=0;fits=0;scalars=0;grid=0;worst=0;max_score=0;max_loglik_error=0
for row in rows:
    fields=row.split()
    if fields[0]=='inverse':
        l,z,a,b=map(exact,fields[1:5]);assert fields[5]=='1',row
        # Quantile inversion signs, at the actual binary64 z (not rounded p).
        slack=512*mp.mpf(2)**-52*(1+z)
        assert h(a,l)<=z+slack and h(b,l)>=z-slack,row
        assert b-a<mp.mpf('3e-14')*(1+b),row
        worst=max(worst,float(max(h(a,l)-z,z-h(b,l),0)/(1+z)))
        count+=1
    elif fields[0]=='fit':
        l=exact(fields[1]);assert fields[3]=='0',row
        m,s,ll,lower,upper=map(exact,fields[4:])
        parts=[ld(mp.mpf(v),m,s,l) for v in [-2,-1,.5,2]]
        truth=sum(v[0] for v in parts)
        assert lower<=truth<=upper,row
        mask=int(fields[2])
        scores=[]
        if not mask&1:scores.append(abs(sum(v[1] for v in parts)))
        if not mask&2:scores.append(abs(sum(v[2] for v in parts)))
        for score in scores:assert score<mp.mpf('1e-6'),row
        max_score=max([max_score]+[float(v) for v in scores])
        max_loglik_error=max(max_loglik_error,float(abs(truth-ll)))
        assert abs(truth-ll)<mp.mpf('1e-10'),row
        fits+=1
    elif fields[0]=='scalar':
        kind=int(fields[1]);assert fields[2]=='0',row
        m,s,l,ll,lower,upper=map(exact,fields[3:9])
        truth=sum(ld(mp.mpf(v),m,s,l)[0] for v in [-2,-1,.5,2])
        assert lower<=truth<=upper,row
        for i in range(21):
            cm=m;cs=s;cl=l
            if kind==0:cm=mp.mpf(-5)+mp.mpf(i)/2
            if kind==1:cs=mp.exp(mp.mpf(-3)+mp.mpf(i)*3/10)
            if kind==2:cl=mp.mpf(-3)+mp.mpf(i)/5
            if kind in (3,4):cm=mp.mpf(-1)+mp.mpf(i)/10
            if kind==4:cs=cl*max(abs(-2-cm),abs(2-cm))
            if kind==5:cs=mp.mpf(6)+mp.mpf(i)/2
            value=sum(ld(mp.mpf(v),cm,cs,cl)[0] for v in [-2,-1,.5,2])
            assert value<=upper,row
            grid+=1
        scalars+=1
identities=0
for l in map(mp.mpf,['2.1','2.5','3','5','30']):
    for u in map(mp.mpf,['.001','.1','.25','.499']):
        v=1-u;d=u**(l-1)+v**(l-1)
        product=(u**l-v**l)*(u**(l-2)-v**(l-2))
        rhs=2*u**(l-1)*v**(l-1)+u**l*v**(l-2)+v**l*u**(l-2)
        assert abs(d*d-product-rhs)<mp.mpf('1e-90')*d*d
        assert -1+(l-1)/l*product/(d*d)<0
        identities+=1
result=dict(precision=mp.mp.dps,inverse_checks=count,fit_checks=fits,scalar_certificates=scalars,independent_grid_checks=grid,positive_shape_identity_checks=identities,
            worst_relative_inverse_sign_deficit=worst,max_free_score=max_score,
            max_loglik_error=max_loglik_error,passed=True)
out=pathlib.Path(__file__).with_name('checks.json');out.write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result))
