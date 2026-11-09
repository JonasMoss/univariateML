"""360-digit GED density/tail references at adjacent binary64 uniform edges."""
import json,math,mpmath as m
m.mp.dps=360
points=[math.nextafter(math.sqrt(3),0),math.sqrt(3),math.nextafter(math.sqrt(3),math.inf)]
rows=[]
for nf in [1e8,1e100,float.fromhex('0x1.fffffffffffffp+1023')]:
    nu=m.mpf(nf);a=1/nu
    ls=(m.loggamma(a)-m.loggamma(3*a))/2
    for xf in points:
        w=m.log(m.mpf(xf))-ls;lx=nu*w
        if lx>m.log(m.mpf(float.fromhex('0x1.fffffffffffffp+1023')))+2:
            ld=lp=-math.inf
        elif lx < -1000:
            # First omitted lower-gamma exponential term <= a*exp(lx)/(1+a).
            logp=w-m.loggamma(1+a)
            lp=-m.log(2)+m.log(-m.expm1(logp))
            ld=-m.log(2)-ls-m.loggamma(1+a)
        else:
            g=m.exp(lx)
            lp=-m.log(2)+m.log(m.gammainc(a,g,m.inf)/m.gamma(a))
            ld=-m.log(2)-ls-m.loggamma(1+a)-g
        rows.append(dict(nu=nf,x=xf,log_density=str(ld),log_upper=str(lp)))
print(json.dumps(rows,indent=2))
