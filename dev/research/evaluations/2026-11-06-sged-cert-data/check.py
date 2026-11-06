"""80-digit independent held-shape reference; run after oracle.cpp writes oracle.txt."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
root=Path(__file__).resolve().parent
x=list(map(mp.mpf,['-3','-1','-.2','0','.3','.4','2']))
n=len(x)
rows=[]
for line in (root/'oracle.txt').read_text().splitlines():
    ns,rs,held,status,cert,ms,ss,ls=line.split()
    nu,xi=mp.mpf(ns),mp.mpf(rs)
    held=int(held)
    # Unstandardized exp(-|z|^nu) moment parameterization.
    m=mp.gamma(2/nu)/mp.gamma(1/nu)
    v=mp.gamma(3/nu)/mp.gamma(1/nu)
    shift=m*(xi-1/xi)
    variance=v*(xi*xi+1/(xi*xi)-1)-shift*shift
    def q(c):
        return sum((abs(t-c)*(xi if t<c else 1/xi))**nu for t in x)
    if nu<=1:
        c=min(x,key=q)
    else:
        a,b=min(x),max(x)
        for _ in range(300):
            c=(a+b)/2
            score=sum(mp.sign(c-t)*abs(c-t)**(nu-1)*(xi**nu if t<c else xi**(-nu)) for t in x)
            if score>0:b=c
            else:a=c
        c=(a+b)/2
    h=mp.mpf('1.7')/mp.sqrt(variance) if held else (nu*q(c)/n)**(1/nu)
    mean=c+h*shift
    sd=h*mp.sqrt(variance)
    ll=n*(mp.log(nu)-mp.loggamma(1/nu)-mp.log(xi+1/xi)-mp.log(h))-q(c)/h**nu
    # The oracle works in mode coordinates, independently of the fitter's helper.
    if int(status)!=0:
        # A numerical stop is permitted by the certificate contract. It must
        # expose no certified estimate; distinguish it from a checked fit.
        stopped=int(status)==9 and int(cert)==0 and mp.isnan(mp.mpf(ms)) and mp.isnan(mp.mpf(ls))
        rows.append(dict(nu=float(nu),xi=float(xi),held_sd=bool(held),status=int(status),outcome='numerical stop',passed=bool(stopped)))
        continue
    me=float(abs(mp.mpf(ms)-mean)/(1+abs(mean)))
    se=float(abs(mp.mpf(ss)-sd)/(1+abs(sd)))
    le=float(abs(mp.mpf(ls)-ll)/(n+abs(ll)))
    passed=int(status)==0 and int(cert)==1 and max(me,se)<2e-8 and le<2e-7
    rows.append(dict(nu=float(nu),xi=float(xi),held_sd=bool(held),status=int(status),outcome='checked fit',mean_error=me,sd_error=se,loglik_error=le,passed=passed))
summary=dict(precision=mp.mp.dps,cases=len(rows),passed=sum(r['passed'] for r in rows),checked_fits=sum(r['status']==0 for r in rows),numerical_stops=sum(r['status']==9 for r in rows),max_mean_error=max(r.get('mean_error',0) for r in rows),max_sd_error=max(r.get('sd_error',0) for r in rows),max_loglik_error=max(r.get('loglik_error',0) for r in rows),rows=rows)
(root/'oracle-results.json').write_text(json.dumps(summary,indent=2)+'\n')
print({k:v for k,v in summary.items() if k!='rows'})
assert all(r['passed'] for r in rows)
