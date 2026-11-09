"""Independent arbitrary-precision gamma integral and Student density oracle.
Run: python3 .../oracle.py > .../oracle.json
No library fitters or truth tables are used or regenerated.
"""
import json
import mpmath as m
m.mp.dps=360
rows=[]
for ns in ['1e8','1e100',str(float.fromhex('0x1.fffffffffffffp+1023'))]:
    nu=m.mpf(float(ns)); a=1/nu
    scale=m.sqrt(m.gamma(a)/m.gamma(3*a))
    moment=m.gamma(2*a)/m.sqrt(m.gamma(a)*m.gamma(3*a))
    shift=moment*m.mpf('1.5')
    sigma=m.sqrt((1-moment**2)*m.mpf('4.25')+2*moment**2-1)
    c=m.gamma((nu+1)/2)/m.gamma(nu/2)/m.sqrt(nu*m.pi)
    cdf=lambda x: m.mpf('.5')+m.quad(lambda t: c*m.exp(-(nu+1)/2*m.log1p(t*t/nu)),[0,x])
    tq=m.findroot(lambda x: cdf(x)-m.mpf('.46875'),(-m.mpf('.08'),-m.mpf('.07')))
    t_moment=2*m.sqrt(nu-2)/(nu-1)*m.gamma((nu+1)/2)/m.gamma(nu/2)/m.sqrt(m.pi)
    t_sigma=m.sqrt((1-t_moment**2)*m.mpf('4.25')+2*t_moment**2-1)
    rows.append(dict(nu=ns,p='.25',qsstd=str((-2*tq*m.sqrt((nu-2)/nu)-m.mpf('1.5')*t_moment)/t_sigma)))
    for ps in ['.01','.25','.75','.99']:
        p=m.mpf(ps)
        # In this interior region x=exp(nu*w) is below exp(-1e6).
        # The omitted first exponential-series term is <= a*x/(a+1).
        w=m.log(abs(2*p-1))+m.loggamma(1+a)
        q=m.sign(p-m.mpf('.5'))*scale*m.exp(w)
        # FS quarter lies on the wide side, with symmetric P=.53125.
        rows.append(dict(nu=ns,p=ps,qged=str(q),density_t_at_1=str(m.gamma((nu+1)/2)/m.gamma(nu/2)/m.sqrt(nu*m.pi)*(1+1/nu)**(-(nu+1)/2))))
    w=m.log(m.mpf('.0625'))+m.loggamma(1+a)
    rows.append(dict(nu=ns,p='.25',qsged=str((2*scale*m.exp(w)-shift)/sigma)))
for ns in ['1e8','1e100',str(float.fromhex('0x1.fffffffffffffp+1023'))]:
    a=1/m.mpf(float(ns))
    for x in [1,2]:
        rows.append(dict(nu=ns,x=x,log_gamma_upper=str(m.log(m.gammainc(a,x,m.inf)/m.gamma(a)))))
print(json.dumps(rows,indent=2))
