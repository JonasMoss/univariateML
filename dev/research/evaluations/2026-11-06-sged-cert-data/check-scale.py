"""80-digit inverse ordinary-scale reference with held mean; independent moments."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
root=Path(__file__).resolve().parent
x=list(map(mp.mpf,['-3','-1','-.2','0','.3','.4','2']))
mean=mp.mpf('.1');n=len(x);rows=[]
for line in (root/'oracle-scale.txt').read_text().splitlines():
    ns,rs,status,cert,ss,ls=line.split()
    nu,xi=mp.mpf(ns),mp.mpf(rs)
    m=mp.gamma(2/nu)/mp.gamma(1/nu)
    v=mp.gamma(3/nu)/mp.gamma(1/nu)
    shift=m*(xi-1/xi)
    variance=v*(xi*xi+1/(xi*xi)-1)-shift*shift
    def score(t):
        total=-mp.mpf(n)
        for a in x:
            z=(a-mean)*t+shift
            total+=nu*(a-mean)*t*mp.sign(z)*abs(z)**(nu-1)*(xi**nu if z<0 else xi**(-nu))
        return total
    a=mp.mpf(0);b=mp.mpf(1)
    while score(b)<0:b*=2
    for _ in range(300):
        c=(a+b)/2
        if score(c)>0:b=c
        else:a=c
    t=(a+b)/2;h=1/t
    sd=h*mp.sqrt(variance)
    penalty=sum((abs((v-mean)*t+shift)*(xi if (v-mean)*t+shift<0 else 1/xi))**nu for v in x)
    ll=n*(mp.log(nu)-mp.loggamma(1/nu)-mp.log(xi+1/xi)-mp.log(h))-penalty
    if int(status)!=0:
        stopped=int(status)==9 and int(cert)==0 and mp.isnan(mp.mpf(ss)) and mp.isnan(mp.mpf(ls))
        rows.append(dict(nu=float(nu),xi=float(xi),status=int(status),outcome='numerical stop',passed=bool(stopped)))
        continue
    se=float(abs(mp.mpf(ss)-sd)/(1+abs(sd)))
    le=float(abs(mp.mpf(ls)-ll)/(n+abs(ll)))
    rows.append(dict(nu=float(nu),xi=float(xi),status=int(status),outcome='checked fit',sd_error=se,loglik_error=le,passed=int(cert)==1 and se<2e-8 and le<2e-8))
summary=dict(precision=mp.mp.dps,cases=len(rows),passed=sum(r['passed'] for r in rows),checked_fits=sum(r['status']==0 for r in rows),numerical_stops=sum(r['status']==9 for r in rows),max_sd_error=max(r.get('sd_error',0) for r in rows),max_loglik_error=max(r.get('loglik_error',0) for r in rows),rows=rows)
(root/'oracle-scale-results.json').write_text(json.dumps(summary,indent=2)+'\n')
print({k:v for k,v in summary.items() if k!='rows'})
assert all(r['passed'] for r in rows)
