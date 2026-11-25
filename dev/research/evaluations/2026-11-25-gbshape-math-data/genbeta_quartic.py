"""Check the centered fourth-order gamma expansion and the explicit raw remainder."""
import json
from pathlib import Path

import mpmath as mp

from gamma_cover import shape
from genbeta_cover import mean

HERE=Path(__file__).resolve().parent


def raw_remainder_bound(q,nu,H,m6):
    d=nu-q*H/2
    B=q*H/(2*nu)
    return (q**7/(2688*(1-B*B)*nu**6)+q**5/(384*(1-B*B)*nu**6)
            +7*q**3/(1152*d**6)+(1+mp.mpf(1)/32)*q/(252*d**6)
            +abs(q-1)*m6/181440)


def run():
    mp.mp.dps=85
    output=[]
    for row in [(".1",".3","1","2.6"),(".5","1.5"),(".01",".1",".5","3.39"),
                (".2",".6","1.1","2.1")]:
        v=list(map(mp.mpf,row))
        avg=mean(v)
        v=[x/avg for x in v]
        c=-mean([mp.log(x) for x in v])
        m2,m4,m6=[mean([x**j for x in v]) for j in (2,4,6)]
        _,seed,_=shape(c)
        k=mp.findroot(lambda x:mp.log(x)-mp.digamma(x)-c,seed,tol=mp.mpf("1e-78"))
        G=lambda q,nu:q*mp.log(nu)-mp.loggamma(q)-(q-1)*c-nu
        C=lambda q,nu:((q-1)*m2+q*(1-q*q)/nu**2)/24
        D=lambda q,nu:q*(1-q*q)*(3*q*q-7)/(960*nu**4)-(q-1)*m4/2880
        J=mp.matrix([[mp.polygamma(1,k),-1/k],[-1/k,1/k]])
        cgrad=mp.matrix([(m2+(1-3*k*k)/k**2)/24,-k*(1-k*k)/(12*k**3)])
        shift=mp.lu_solve(J,cgrad)
        fourth=D(k,k)+(cgrad.T*shift)[0]/2
        vals=[]
        def vlik(h,p,q):
            r=mean([mp.log(-mp.expm1(-h*x)) for x in v])
            return mp.log(h)-p*h+(q-1)*r-mp.log(mp.beta(p,q))

        H=mp.mpf(".02")
        for h in [H,H/10,H/100]:
            for q,nu in [(k,k),(k*mp.mpf(".9"),k*mp.mpf("1.1")),
                         (k*mp.mpf("1.1"),k*mp.mpf(".9"))]:
                p=nu/h-(q-1)/2
                approx=G(q,nu)+h**2*C(q,nu)+h**4*D(q,nu)
                assert abs(vlik(h,p,q)-approx) <= h**6*raw_remainder_bound(q,nu,H,m6)
            r=mean([mp.log(-mp.expm1(-h*x)) for x in v])
            def scores(p,q):
                g=mp.digamma(p+q)
                return g-mp.digamma(p)-h,g-mp.digamma(q)+r
            p,q=mp.findroot(scores,(k/h-(k-1)/2,k),tol=mp.mpf("1e-70"))
            F=vlik(h,p,q)
            error=abs((F-G(k,k)-h**2*C(k,k))/h**4-fourth)
            vals.append(error)
        assert vals[-1]<vals[0]/1000
        output.append(dict(log_distances=row,k=k,c2=C(k,k),c4=fourth,errors=vals))
    (HERE/"genbeta-quartic-results.json").write_text(
        json.dumps(dict(precision=85,checks=output),indent=2,default=lambda x:mp.nstr(x,28))+"\n")
    print("36 raw sixth-order bounds and four profiled fourth-order expansions passed")


if __name__=="__main__":
    run()
