import mpmath as mp,json
from pathlib import Path
mp.mp.dps=420
root=Path(__file__).resolve().parent
checks=[]
for line in (root/'inverse.log').read_text().splitlines():
    l,z,ld,ok,s,d,old=line.split();l,z,ld,s,d=map(lambda v:mp.mpf(float(v)),(l,z,ld,s,d))
    assert ok=='1',line
    deficit=l>0 and ld<-mp.log(2)
    if z==0:t=-mp.log(2)
    elif l==0:t=-z-mp.log1p(mp.exp(-z))
    else:
        # Monotone independent bisection, no implementation/table seed.
        def fun(t):
            p=mp.exp(t);q=1-p
            if deficit:return mp.log(p**l-mp.expm1(l*mp.log1p(-p)))-ld
            return mp.log((q**l-p**l)/l)-mp.log(z)
        a=mp.mpf(-2);b=-mp.log(2)
        while (fun(a)>0)==deficit:a*=2
        for i in range(1700):
            t=(a+b)/2;v=fun(t)
            if (v>0)==deficit:b=t
            else:a=t
            if b-a<mp.mpf('1e-70')*(1+abs(t)):break
    refd=mp.log(mp.exp((l-1)*t)+mp.exp((l-1)*mp.log1p(-mp.exp(t))))
    err=abs(d-refd)/(1+abs(refd));se=abs(s-t)/(1+abs(t))
    assert err<mp.mpf('5e-13') and se<mp.mpf('5e-13'),(line,mp.nstr(err),mp.nstr(se))
    checks.append(dict(l=float(l),z=str(z),logdeficit=str(ld),logp=str(t),logD=str(refd),error=float(err),logp_error=float(se)))
(root/'inverse-checks.json').write_text(json.dumps(checks,indent=2)+'\n')
print('PASS',len(checks),'max normalized logD error',max(v['error'] for v in checks),'max logp error',max(v['logp_error'] for v in checks))
