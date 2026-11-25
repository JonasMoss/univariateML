"""Exact-arithmetic interval bounds, evaluated numerically with mpmath."""
import bisect
import json
from functools import cache
from pathlib import Path

import mpmath as mp

from gamma_cover import golden, shape

HERE = Path(__file__).resolve().parent


def mean(xs):
    return mp.fsum(xs)/len(xs)


def info(p,q):
    common=mp.polygamma(1,p+q)
    return mp.polygamma(1,p)-common,mp.polygamma(1,q)-common,common


def setup(values, trial_h, trial_p, trial_q):
    v=[y/mean(values) for y in values]
    n=len(v)
    c=-mean([mp.log(y) for y in v])
    delta=1-min(v)
    pad=mp.mpf("1e-26")
    seeds=[(trial_h,trial_p,trial_q)]

    @cache
    def stats(h):
        w=[mp.exp(-h*x) for x in v]
        z=[-mp.expm1(-h*x) for x in v]
        return (mean([-mp.log1p(-u) if u<mp.mpf(".5") else -mp.log(w0)
                       for u,w0 in zip(w,z)]),
                mean([x*u/w0 for x,u,w0 in zip(v,w,z)]),
                mean([x*x*u/w0**2 for x,u,w0 in zip(v,w,z)]))

    def scores(h,r,p,q):
        u=mp.digamma(p+q)
        return u-mp.digamma(p)-h,u-mp.digamma(q)-r

    @cache
    def point(h):
        r,D,K=stats(h)
        i=bisect.bisect_left(seeds,(h,))
        h0,p,q=min(seeds[max(0,i-1):i+1],key=lambda x:abs(mp.log(x[0]/h)))
        p=p*h0/h
        q=q*stats(h0)[0]/r
        for _ in range(80):
            g,j=scores(h,r,p,q)
            A,B,T=info(p,q)
            det=A*B-T*T
            dp,dq=(B*g+T*j)/det,(T*g+A*j)/det
            if max(abs(dp/p),abs(dq/q))<mp.mpf("1e-34"):
                break
            shrink=min(1,mp.mpf(".5")/max(abs(dp/p),abs(dq/q)))
            p,q=p+shrink*dp,q+shrink*dq
        else:
            raise ArithmeticError(f"conditional shape solve failed at {h}")
        bisect.insort(seeds,(h,p,q))
        cp=h+mp.log(-mp.expm1(-r))
        cq=r+mp.log(-mp.expm1(-h))
        pL,pH=mp.exp(-r)/h,1/cp
        qL,qH=mp.exp(-h)/r,1/cq
        F=mp.log(h)-p*h-(q-1)*r-mp.log(mp.beta(p,q))
        upper=F+max(g*(pL-p),g*(pH-p))+max(j*(qL-q),j*(qH-q))
        return p,q,F,upper,1/h-p+(q-1)*D

    def rectangle(l,u):
        center=mp.sqrt(l*u)
        r=stats(center)[0]
        rl,ru=stats(l)[0],stats(u)[0]
        p,q,*_=point(center)
        g,j=scores(center,r,p,q)
        Dp=max(center-l,u-center)+abs(g)+pad
        Dq=max(rl-r,r-ru)+abs(j)+pad
        A,B,T=info(p,q)
        det=A*B-T*T
        wp=2*(B*Dp+T*Dq)/det
        wq=2*(T*Dp+A*Dq)/det
        pL,pH,qL,qH=p-wp,p+wp,q-wq,q+wq
        if min(pL,qL)<=0:
            return None
        # The worst corners for the p and q signs coincide by monotonicity.
        low=scores(u,rl,pL,qL)
        high=scores(l,ru,pH,qH)
        if min(low)>pad and max(high)<-pad:
            return pL,pH,qL,qH
        return None

    return v,c,delta,pad,stats,point,rectangle


def signed_gamma_seam(v,A,qlo,qhi,cost,kL,kH,gap=0):
    m2=mean([x*x for x in v])
    m4=mean([x**4 for x in v])
    c2=lambda k:(k-1)*(m2-1-1/k)/24
    upper=max(c2(kL),c2(kH))
    assert upper<0 or gap>0
    D=cost+kH/6+max(0,1-kL)*m2/24
    H=A
    for _ in range(100):
        low=qlo*H/mp.expm1(H)-H/2
        high=qhi+H*(qhi+1)/2
        radi_q=2*max(qhi,kH)*mp.sqrt(D)*H
        radi_n=high*mp.sqrt(2*D/qlo)*H
        ql,qh=max(qlo,kL-radi_q),min(qhi,kH+radi_q)
        nl,nh=max(low,ql-radi_n),min(high,qh+radi_n)
        if nl>qh*H/2:
            for _ in range(12):
                width=nl-qh*H/2
                ratio=(qh*H/(2*nl))**2
                bn=qh/(12*width**3)+qh**3/(12*nl**3*(1-ratio))
                bq=qh**2/(8*nl**2*(1-ratio))+1/(24*width**2)+m2/24
                rn=nh*bn*H*H
                rq=2*max(qh,kH)**2*(bq+nh*bn/min(nl,ql))*H*H
                ql,qh=max(ql,kL-rq),min(qh,kH+rq)
                nl,nh=max(nl,ql-rn),min(nh,qh+rn)
            Lq=(m2-1+1/min(ql,kL)**2)/24
            cands=[ql,qh]
            turn=1/mp.sqrt(3)
            if ql<turn<qh:
                cands.append(turn)
            Ln=max(abs(q-q**3) for q in cands)/(12*min(nl,ql)**3)
            R=qh**3/(96*nl**2*(nl**2-qh**2*H**2/4))+max(0,1-ql)*m4/2880
            quartic=max(qh,kH)**2*Lq**2+max(nh,qh)**2*Ln**2/(2*ql)+R
            normalized=upper+quartic*H*H
            if (normalized < -mp.mpf("1e-24") if not gap else
                    max(0,normalized)*H*H < gap/2):
                return H,upper,quartic
        H/=2
    raise ArithmeticError("signed gamma neighborhood failed")


def cover(values, initial, allowance=12000, gamma=False):
    v,c,delta,pad,stats,point,rectangle=setup(values,*initial)
    n=len(v)
    h0=initial[0]
    kL,k,kH=shape(c)
    G=k*mp.log(k)-k-mp.loggamma(k)-(k-1)*c
    Gupper=G+max(abs(k-kL),abs(kH-k))*abs(mp.log(k)-mp.digamma(k)-c)+pad
    W=-mp.log(delta)-1
    m2=mean([x*x for x in v])
    A=min(c/2,mp.log(mp.mpf(4)/3),mp.log1p(1/(4*c)),mp.sqrt(12*c/m2))
    qlo=(mp.exp(-A)-mp.mpf(".5"))/c
    qhi=1/(c-A*A*m2/24)
    factor=1-mp.expm1(A)/(2*qlo)
    Csmall=(mp.expm1(A)/A)**2/(8*qlo*factor**2)+max(0,qhi-1)*m2/24
    if gamma:
        witness=G-pad
        assert witness>W
        root=l=r=None
        T,c2,c4=signed_gamma_seam(v,A,qlo,qhi,Csmall,kL,kH)
        scale=2*T
    else:
        _,_,F,_,_=point(h0)
        witness=F-pad
        if witness<=max(Gupper,W):
            return dict(closed=False,reason="finite proposal does not separate both faces",
                        witness=witness,gamma=G,power=W,cells=0)
        # Refine the proposal on the profile. No proposal score is used for exclusion.
        root=mp.findroot(lambda h:point(h)[-1],(h0*mp.mpf(".999"),h0*mp.mpf("1.001")))
        witness=point(root)[2]-pad
        l=root*mp.mpf(".9999998")
        r=root*mp.mpf("1.0000002")
        T=min(A,mp.sqrt((witness-Gupper)/(2*Csmall)),l/2)
        improved,*_=signed_gamma_seam(v,min(A,l/2),qlo,qhi,Csmall,kL,kH,witness-Gupper)
        T=max(T,improved)
        scale=2*r
    H=max(scale,mp.log(2)/min(v),mp.log(4*n)/delta,(mp.log(n)+4)/delta)
    while W+(mp.log(4*n+1)+mp.euler)/(H*delta-mp.log(n)-1)-mp.log1p(-mp.exp(-H*min(v)))>=witness:
        H*=2
    # Use uniform signed large-tail criterion when it improves the cutoff.
    Hp=max(scale,mp.log(2)/min(v),mp.log(4*n)/delta,(mp.log(n)+4)/delta,1/min(v))
    C=mp.log(n)+mp.euler
    while Hp < H:
        B=1/(Hp*delta-mp.log(n)-1)
        if B<mp.exp(-C-1) and 2*mp.e*Hp*mp.exp(-Hp*min(v)) < mp.log(Hp)-C+1:
            H=Hp
            break
        Hp*=mp.mpf("1.25")
    counts=dict(cells=0,chord=0,centered_chord=0,monotone=0,curvature=0,q_eliminated=0)
    stack=[(T,H,False)] if gamma else [(T,l,False),(r,H,False),(l,r,True)]
    def score_interval(h):
        pL,pH,qL,qH=rectangle(h,h)
        D=stats(h)[1]
        return 1/h-pH+(qL-1)*D-pad,1/h-pL+(qH-1)*D+pad

    while stack:
        left,right,curvature=stack.pop()
        counts["cells"]+=1
        if counts["cells"]%512==0:
            print("genbeta compact cells",counts,"next",mp.nstr(left,8),mp.nstr(right,8),curvature,flush=True)
        if counts["cells"]>allowance:
            return dict(closed=False,reason="cell allowance",root=root,witness=witness,
                        gamma=G,power=W,seam=T,tail=H,remaining=len(stack)+1,**counts)
        rl,Dl,Kl=stats(left)
        ru,Du,_=stats(right)
        width=right-left
        if not curvature and rl<1:
            gap=left+mp.log(ru)-rl
            if gap>0:
                kl,k,kh=shape(gap)
                M=k*(mp.log(k)-1-gap)-mp.loggamma(k)
                resid=mp.log(k)-mp.digamma(k)-gap
                upper=mp.log(right)+rl+M+max(resid*(kl-k),resid*(kh-k))
                if upper+pad<witness:
                    counts["q_eliminated"]+=1
                    continue
        if not curvature and right<mp.log(2) and c-right**2*m2/24>0:
            qlow=(mp.exp(-right)-mp.mpf(".5"))/c
            qhigh=1/(c-right**2*m2/24)
            rate_upper=qhigh+(qhigh+1)*right/2
            base=right/mp.expm1(right)-right/2
            d=qlow*base-right/2
            if base>0 and d>0:
                M=qhigh**3/(12*d*d)+qhigh*rate_upper/(6*d**3)+max(0,1-qlow)*m2/12
                if max(point(left)[3],point(right)[3])+M*width**2/8+pad<witness:
                    counts["centered_chord"]+=1
                    continue
        cq=ru+mp.log(-mp.expm1(-left))
        # Conditionally maximized q <= 1/cq. Chord excess of each smooth
        # term is bounded by a uniform bound on its negative curvature.
        if not curvature and cq>pad and right/left<2:
            gap=(1/left**2+max(0,1/cq-1)*Kl)*width**2/8
            pl=point(left)[3]
            pr=point(right)[3]
            if max(pl,pr)+gap+pad<witness:
                counts["chord"]+=1
                continue
        box=rectangle(left,right) if right/left<mp.mpf("1.08") else None
        if box:
            pL,pH,qL,qH=box
            sl=1/right-pH+(qL-1)*(Du if qL>=1 else Dl)
            sh=1/left-pL+(qH-1)*(Dl if qH>=1 else Du)
            if not curvature and (sl>pad or sh<-pad):
                counts["monotone"]+=1
                continue
            Amax=info(pL,qH)[0]
            Bmax=info(qL,pH)[0]
            Amin=info(pH,qL)[0]
            Bmin=info(qH,pL)[0]
            Tmin=mp.polygamma(1,pH+qH)
            Tmax=mp.polygamma(1,pL+qL)
            det=Amin*Bmin-Tmax**2
            if det>0:
                Ku=stats(right)[2]
                qterm=(qL-1)*(Ku if qL>=1 else Kl)
                upper=-1/right**2-qterm+(Bmax+Amax*Dl**2-2*Tmin*Du)/det+pad
                if curvature and upper<-pad:
                    counts["curvature"]+=1
                    continue
                if not curvature:
                    if upper<0 and (score_interval(left)[1]<0 or score_interval(right)[0]>0):
                        counts["monotone"]+=1
                        continue
                    qterm=(qH-1)*(Kl if qH>=1 else Ku)
                    lower=-1/left**2-qterm+max(0,Bmin+Amin*Du**2-2*Tmax*Dl)/(Amax*Bmax-Tmin**2)-pad
                    mid=mp.sqrt(left*right)
                    sm,sp=score_interval(mid)
                    neg,pos=mid-left,right-mid
                    if (sm+min(0,-upper*neg,lower*pos)>pad
                            or sp+max(0,-lower*neg,upper*pos)<-pad):
                        counts["monotone"]+=1
                        continue
            if not curvature and min(point(left)[3]+max(0,sh)*width,
                       point(right)[3]+max(0,-sl)*width)+pad<witness:
                counts["chord"]+=1
                continue
        mid=mp.sqrt(left*right)
        stack.extend(((left,mid,curvature),(mid,right,curvature)))
    if gamma:
        return dict(closed=True,witness=witness,gamma=G,power=W,seam=T,tail=H,
                    quadratic=c2,fourth_bound=c4,points=point.cache_info().currsize,**counts)
    # Validate signs by applying the conditional rectangle to each singleton.
    signs=[score_interval(h) for h in (l,r)]
    assert signs[0][0]>pad and signs[1][1]<-pad
    return dict(closed=True,root=root,slab=[l,r],signs=signs,witness=witness,
                gamma=G,power=W,seam=T,tail=H,points=point.cache_info().currsize,**counts)


def quantile_fixture(n,p,q,a):
    # Mathematical mid-quantiles of Beta(p,q) transformed by u^(1/a).
    values=[]
    for i in range(n):
        target=mp.mpf(2*i+1)/(2*n)
        lo,hi=mp.mpf(0),mp.mpf(1)
        for _ in range(95):
            mid=(lo+hi)/2
            if mp.betainc(p,q,0,mid,regularized=True)<target:
                lo=mid
            else:
                hi=mid
        values.append(-mp.log((lo+hi)/2)/a)
    return values


def run():
    mp.mp.dps=60
    payload=golden("genbeta")
    values=[-mp.log(mp.mpf(x)) for x in payload["data"]]
    ep=payload["estimates"]
    result=dict(precision=mp.mp.dps)
    result["golden"]=cover(values,(mp.mpf(ep["shape3"])*mean(values),mp.mpf(ep["shape1"]),mp.mpf(ep["shape2"])))
    print("genbeta golden completed",result["golden"]["closed"],result["golden"]["cells"],flush=True)
    hard=quantile_fixture(300,3,2,mp.mpf(".7"))
    result["quantile_3_2_07"]=cover(hard,(mp.mpf(".41118368148")*mean(hard),mp.mpf("5.364578"),mp.mpf("1.9924355")))
    print("genbeta hard cover completed",result["quantile_3_2_07"]["closed"],flush=True)
    n=300
    v=[mp.mpf(".05")]+[mp.mpf(".5")+(i-mp.mpf(".5"))/(n-1) for i in range(1,n)]
    _,k,_=shape(-mean([mp.log(x/mean(v)) for x in v]))
    result["gamma_300_005"]=cover(v,(mp.mpf(".04"),k/mp.mpf(".04"),k),gamma=True)
    print("genbeta gamma fixture completed",result["gamma_300_005"]["closed"],flush=True)
    v=[mp.mpf(".5"),mp.mpf("1.5")]
    c=-mean([mp.log(x) for x in v])
    _,k,_=shape(c)
    G=k*mp.log(k)-k-mp.loggamma(k)-(k-1)*c
    W=mp.log(2)-1
    c2=(k-1)*(mean([x*x for x in v])-1-1/k)/24
    assert c2<0 and G<W
    result["strict_trough"]=dict(v=v,k=k,G=G,W=W,c2=c2)
    (HERE/"genbeta-results.json").write_text(json.dumps(result,indent=2,default=lambda x:mp.nstr(x,28))+"\n")


if __name__=="__main__":
    run()
