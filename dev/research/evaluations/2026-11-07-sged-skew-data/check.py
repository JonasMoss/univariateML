"""Independent arbitrary-precision knot/face comparison and moment identity.
Run with /tmp/umvl-cpu/run python3 <this file>. No C++ formulas imported.
"""
import json
import mpmath as m
m.mp.dps=70
records=[]
for shape in ['.3','.5','.8','1']:
    nu=m.mpf(shape)
    for data in [[-1,1],[-4,-1,0,0,0,0,0,0,.3,2],[-100]+[-1]*5+[1]*5+[100]]:
        x=list(map(m.mpf,map(str,data)));n=len(x);rows=[]
        for c in sorted(set(x)):
            A=sum(abs(z-c)**nu for z in x if z<c)
            B=sum(abs(z-c)**nu for z in x if z>c)
            ar=A**(1/(nu+1));br=B**(1/(nu+1));S=ar+br
            t=(nu*S/n)**(1/nu);a=t*ar;b=t*br
            ll=n*(m.log(nu)-m.loggamma(1/nu)-m.log(a+b)-1/nu)
            rows.append({'join':str(c),'a':str(a),'b':str(b),'logLik':str(ll)})
            if a and b:
                k=m.gamma(2/nu)/m.gamma(1/nu);v=m.gamma(3/nu)/m.gamma(1/nu)
                mean=c+k*(b-a);sd=m.sqrt(v*(a*a-a*b+b*b)-k*k*(b-a)**2)
                xi=m.sqrt(b/a);base=m.sqrt(m.gamma(1/nu)/m.gamma(3/nu))
                shift=k*base*(xi-1/xi);su=sd*base/m.sqrt(a*b)
                actual=0
                for z in x:
                    q=(z-mean)*su/sd+shift
                    q/=xi if q>0 else 1/xi
                    actual+=m.log(su/sd)+m.log(nu)-m.log(base)-m.log(xi+1/xi)-m.loggamma(1/nu)-(abs(q)/base)**nu
                # A cusp residual is zero in exact arithmetic; arbitrary precision
                # rounding is amplified by nu. 70 digits suffices for 1e-17 here.
                assert abs(actual-ll)<m.mpf('1e-17')
        best=max(m.mpf(r['logLik']) for r in rows)
        winners=[r for r in rows if abs(m.mpf(r['logLik'])-best)<m.mpf('1e-30')]
        # Independent grid on each open join interval: concavity bound regression.
        for left,right in zip(sorted(set(x)),sorted(set(x))[1:]):
            for j in range(1,100):
                c=left+(right-left)*j/100
                A=sum(abs(z-c)**nu for z in x if z<c);B=sum(abs(z-c)**nu for z in x if z>c)
                S=A**(1/(nu+1))+B**(1/(nu+1))
                ll=n*(m.log(nu)-m.loggamma(1/nu)-(m.log(nu*S/n)/nu)-m.log(S)-1/nu)
                assert ll<=best+m.mpf('1e-60')
        records.append({'nu':shape,'data':data,'winners':winners})
contacts=[]
for shape in ['.5','1','1.7','2','4']:
    nu=m.mpf(shape);k=m.gamma(2/nu)/m.gamma(1/nu);v=m.gamma(3/nu)/m.gamma(1/nu)
    h=v-k*k;D=3*v-4*k*k;z=1-m.mpf('1e-25')
    # Hold mean=k and variance=h: the right half-GED face has c=0,b=1.
    total=2*m.sqrt(h)/m.sqrt(v+D*z*z)
    a=total*(1-z)/2;b=total*(1+z)/2;c=k-k*z*total
    ratio=abs(c/a);expected=k*v/(2*h)
    assert abs(ratio/expected-1)<m.mpf('1e-23')
    contacts.append({'nu':shape,'contact_residual_ratio':str(ratio),
                     'limit_ratio':str(expected),'contact_penalty':str(expected**nu)})
print(json.dumps({'precision':m.mp.dps,'records':records,'held_moment_contacts':contacts},indent=2))
