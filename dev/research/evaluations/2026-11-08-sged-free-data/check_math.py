#!/usr/bin/env python3
"""Independent high-precision checks; numerical validation, not proof.
Run from repository root through /tmp/umvl-cpu/run.
"""
import csv,json
from pathlib import Path
import mpmath as m
m.mp.dps=70
out=Path(__file__).resolve().parent
M=lambda x:m.mpf(x)  # floats are imported exactly, not via decimal rounding
rows=list(csv.DictReader(out.joinpath('after-golden.csv').open()))[:4]
x=list(map(M,json.load(open('univariateML-cpp/include/cppunivariate/distributions/sged/golden.json'))['data']))
roots=[]
for row in rows:
    mask=int(row['held_mask']); hm=bool(mask&1);hs=bool(mask&2)
    def values(mu,p):
        residual=[abs(t-mu) for t in x]
        T=m.fsum(r**p for r in residual)
        b=M(1.4)*m.sqrt(m.gamma(1/p)/m.gamma(3/p)) if hs else (p*T/len(x))**(1/p)
        U=m.fsum(r**p*m.log(r) if r else 0 for r in residual)
        beta=(3*m.digamma(1+3/p)-m.digamma(1+1/p))/2
        A=(m.digamma(1+1/p)-beta+(p*beta*T/b**p-p*p*(U-T*m.log(b))/b**p)/len(x)) if hs else m.digamma(1+1/p)+m.log(p)-p*U/T+m.log(T/len(x))
        location=m.fsum(m.sign(mu-t)*abs(mu-t)**(p-1) for t in x)
        return A,location,b,T
    if hm:
        p=m.findroot(lambda p:values(M(.2),p)[0],M(row['shape']),tol=m.mpf('1e-55'))
        mu=M(.2)
    else:
        mu,p=m.findroot(lambda mu,p:(values(mu,p)[1],values(mu,p)[0]),(M(row['mean']),M(row['shape'])),tol=m.mpf('1e-55'))
    A,loc,b,T=values(mu,p)
    sd=M(1.4) if hs else b*m.sqrt(m.gamma(3/p)/m.gamma(1/p))
    ll=len(x)*(m.log(p)-m.log(2)-m.log(b)-m.loggamma(1/p))-T/b**p
    error=max(abs(mu-M(row['mean'])),abs(sd-M(row['sd'])),abs(p-M(row['shape'])))
    assert error<m.mpf('3e-7'),(mask,error)
    roots.append(dict(mask=mask,mean=str(mu),sd=str(sd),nu=str(p),loglik=str(ll),max_coefficient_error=float(error),loglik_error=float(abs(ll-M(row['loglik'])))))
# Shape/skew joint corners, exact pinned mean=0 and sd=1, contact x=+-sqrt(3).
# z=(b-a)/(a+b), k=G(2/p)/G(1/p), v=G(3/p)/G(1/p).
corner=[]
for lam in [1,10,100]:
    for p in [m.mpf('1e3'),m.mpf('1e5'),m.mpf('1e7')]:
        z=1-lam/p
        k=m.gamma(2/p)/m.gamma(1/p);v=m.gamma(3/p)/m.gamma(1/p)
        D=3*v-4*k*k;s=2/m.sqrt(v+D*z*z);c=-k*z*s
        a=s*(1-z)/2;b=s*(1+z)/2
        left=p*m.log((c+m.sqrt(3))/a);right=p*m.log((m.sqrt(3)-c)/b)
        target=-m.euler+2*m.zeta(2)/lam
        corner.append(dict(lam=lam,nu=float(p),left_log_penalty=str(left),left_limit=str(target),right_log_penalty=str(right),right_limit=str(-m.euler)))
        if p==m.mpf('1e7'):
            assert abs(left-target)<m.mpf('1e-4');assert abs(right+m.euler)<m.mpf('1e-4')
# Identity of the actual moment map and kernel likelihood, across every held mask.
identity=[]
for p in map(M,[.5,1,1.7,2,4,100]):
    for xi in map(M,[.1,.7,1,1.4,10]):
        k=m.gamma(2/p)/m.gamma(1/p);v=m.gamma(3/p)/m.gamma(1/p)
        for mu in map(M,[0,.2]):
            for sd in map(M,[1,1.4]):
                a=sd/m.sqrt(v*(1-xi*xi+xi**4)-k*k*(xi*xi-1)**2);b=a*xi*xi;c=mu-k*(b-a)
                assert abs(c+k*(b-a)-mu)<m.mpf('1e-60')
                assert abs(v*(a*a-a*b+b*b)-k*k*(b-a)**2-sd*sd)<m.mpf('1e-60')
                # Reconstruct the standardized FS density independently.
                m1=k/m.sqrt(v);shift=m1*(xi-1/xi)
                su=m.sqrt((1-m1*m1)*(xi*xi+1/(xi*xi))+2*m1*m1-1)
                for y in map(M,[-3,-1,0,.1,2,4]):
                    z=(y-mu)*su/sd+shift;chi=xi if z>0 else 1/xi
                    base=1/m.sqrt(v)
                    fs=m.log(su/sd)+m.log(2/(xi+1/xi))+m.log(p/(2*base*m.gamma(1/p)))-abs(z/(chi*base))**p
                    direct=m.log(p)-m.loggamma(1/p)-m.log(a+b)-(abs(y-c)/(a if y<c else b))**p
                    assert abs(fs-direct)<m.mpf('1e-45')*(1+abs(direct))
                identity.append(dict(nu=float(p),xi=float(xi),mean=float(mu),sd=float(sd)))
# Fixed-skew, free-moment tail bound: T >= rho**p, rho=range/(xi+1/xi).
# ell/n <= -log(range) - [log(p)+1-gamma-log(n)]/p.
tail=[]
y=list(map(M,[-3,-1,0,.1,2,4]));n=len(y)
for p in map(M,[10,20,100,1000]):
    for xi in list(map(M,[.1,.7,1,1.4,10]))+[p,p*p]:
        rho=(max(y)-min(y))/(xi+1/xi)
        # Convex mode root; bisect independently, no production helper.
        lo=min(y);hi=max(y)
        for j in range(250):
            c=(lo+hi)/2
            score=m.fsum((xi*(c-t))**(p-1)*xi if t<c else -((t-c)/xi)**(p-1)/xi for t in y)
            if score>0:hi=c
            else:lo=c
        c=(lo+hi)/2
        T=m.fsum((xi*(c-t))**p if t<c else ((t-c)/xi)**p for t in y)
        assert T>=rho**p*(1-m.mpf('1e-60'))
        ll=m.log(p)-m.loggamma(1/p)-m.log(xi+1/xi)-(m.log(p*T/n)+1)/p
        upper=-m.log(max(y)-min(y))-(m.log(p)+1-m.euler-m.log(n))/p
        assert ll<=upper+m.mpf('1e-55')
        tail.append(dict(nu=float(p),xi=float(xi),loglik_per_observation=str(ll),upper=str(upper)))
record=dict(precision=m.mp.dps,roots=roots,joint_corner=corner,moment_identity_rows=len(identity),tail=tail,violations=0)
out.joinpath('math-checks.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(dict(roots=len(roots),moment_identity_rows=len(identity),tail_rows=len(tail),corner_rows=len(corner),max_coefficient_error=max(r['max_coefficient_error'] for r in roots),violations=0)))
