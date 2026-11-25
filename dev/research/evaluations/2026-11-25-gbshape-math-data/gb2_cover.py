"""Mode-centered GB2 power/shape boxes and a small-fixed-power face test."""
import gzip
import heapq
import json
from itertools import count
from pathlib import Path

import mpmath as mp
import numpy as np

from gamma_cover import fit_side, golden, shape, standardized

HERE = Path(__file__).resolve().parent
PAD=mp.mpf("1e-27")


def mean(z):
    return mp.fsum(z)/len(z)


def height(k,r):
    """Log height in power-scaled coordinates and concentration score."""
    if not r or r==1:
        return k*(mp.log(k)-1)-mp.loggamma(k), mp.log(k)-mp.digamma(k)
    p,q=k/(1-r),k/r
    s=p+q
    value=p*mp.log(r)+q*mp.log1p(-r)-mp.log(mp.beta(p,q))
    f=lambda x:mp.log(x)-mp.digamma(x)
    return value,(p*f(p)+q*f(q)-s*f(s))/k


def kernel(z,r):
    if z<0:
        f,g=kernel(-z,1-r)
        return f,-g
    w=mp.expm1(z)
    if r==0:
        return w-z,w
    if r==1:
        return z+mp.expm1(-z),-mp.expm1(-z)
    return (mp.log1p(r*w)-r*z)/(r*(1-r)), w/(1+r*w)


def minimized_loss(y,a,l,r,grid=None):
    """Convex lower loss for every parameter in this ratio cell at power >= a."""
    lo,hi=min(y),max(y)
    m=(lo+hi)/2
    if grid is not None:
        lo0,hi0=float(lo),float(hi)
        af,lf,rf=float(a),float(l),float(r)
        for _ in range(56):
            mf=(lo0+hi0)/2
            z=np.clip(af*(grid-mf),-690.0,690.0)
            v=np.exp(-abs(z))
            # Float64 is only a proposal for the convex supporting tangent.
            slope=np.sum((-np.expm1(-abs(z)))*np.where(z>=0,1.0,-1.0)
                         /np.where(z>=0,rf+(1-rf)*v,1-lf+lf*v))
            if slope>0:
                lo0=mf
            else:
                hi0=mf
        m=mp.mpf((lo0+hi0)/2)
    for _ in range(1 if grid is not None else 18):
        at=m
        terms=[kernel(a*(x-m),r if x>=m else l) for x in y]
        F=mean([v for v,_ in terms])
        g=-a*mean([s for _,s in terms])
        if abs(g)*(hi-lo)<PAD:
            break
        if g<0:
            lo=m
        else:
            hi=m
        # Differentiated normalized logistic curvature.
        H=a*a*mean([mp.exp(a*(x-m))/(1+(r if x>=m else l)*mp.expm1(a*(x-m)))**2
                    for x in y])
        trial=m-g/H
        m=trial if lo<trial<hi else (lo+hi)/2
    lower=F+min(g*(lo-at),g*(hi-at))-PAD
    return lower,F


def value_bound(y,lo,hi,l,r,W,grid=None):
    d,_=minimized_loss(y,lo,l,r,grid)
    if d<=0:
        return mp.inf
    ratio=(l+r)/2
    # Curvature bound from Stirling gives a cheap screen.
    span=max(y)-min(y)
    shift=(hi-lo)*span
    upper=min(mp.log(hi)-mp.log(4*mp.pi*mp.e*d)/2,
              mp.log(lo)-mp.log(4*mp.pi*mp.e*d)/2+shift/2,
              mp.log(lo)-mp.log(d)-1)
    if upper+PAD<W:
        return upper+PAD
    kmin=2*mp.pi*mp.exp(2*W)/hi**2
    error=(r-l)*min(1,1/(8*kmin))
    k=mp.mpf(".65")/d
    a,b=mp.mpf(0),1/d
    for _ in range(35):
        F,g=height(k,ratio)
        g-=d
        tangent=F-k*d+max(g*(a-k),g*(b-k))
        if abs(g)*b<PAD or mp.log(hi)+tangent+error+PAD<W:
            break
        if g>0:
            a=k
        else:
            b=k
        k=(a+b)/2
    return min(upper,max(W,mp.log(hi)+tangent+error))+PAD


def peak_limit(y,W):
    mid=sorted(y)[len(y)//2]
    mad=mean([abs(x-mid) for x in y])
    cap=2/mad
    while mp.log(cap)+1-cap*mad>=W:
        cap*=2
    return cap


def normal_tail(y,W):
    normal=-mp.log(2*mp.pi*mp.e)/2
    assert W>normal
    R=max(y)-min(y)
    A=(W-normal)/R
    bound=normal+A*R/2
    for _ in range(32):
        candidate=2*A
        d,_=minimized_loss(y,candidate,mp.mpf(0),mp.mpf(1))
        trial=mp.log(candidate)-mp.log(4*mp.pi*mp.e*d)/2
        if trial+PAD>=W:
            break
        A,bound=candidate,trial+PAD
    return A,bound


def asymmetric_laplace(y):
    best=-mp.inf
    for m in sorted(set(y)):
        left=mean([max(0,m-x) for x in y])
        right=mean([max(0,x-m) for x in y])
        best=max(best,-1-2*mp.log(mp.sqrt(left)+mp.sqrt(right)))
    return best


def large_tail(y,W):
    AL=asymmetric_laplace(y)
    assert W>AL
    M=peak_limit(y,W)
    delta=min(mp.mpf(".5"),mp.sqrt((W-AL)/(4*mp.polygamma(1,1))))
    small=AL+mp.polygamma(1,1)*delta**2
    # Choose shift from the smaller of the two endpoint mean distances.
    gap=min(max(y)-mean(y),mean(y)-min(y))
    eps=gap*min(mp.mpf(".1"),(W-AL)/8)
    A=max(4*M/delta,1/eps)

    def one_side(z):
        S=min(mp.exp(W)*mp.expm1(A*eps)/2,A*delta*(-mp.expm1(-A*eps))/2)
        C=(1+4*M/(A*delta))/A
        ymin,ymax=min(z),max(z)
        knots=set(z)|{t-eps for t in z if ymin<t-eps<ymax}
        upper=-mp.inf
        for m in knots:
            L=mean([max(0,m-x) for x in z])-C
            R=mean([max(0,x-m-eps) for x in z])
            rate=min(4*M,max(mp.exp(W),1/L)) if L>0 else 4*M
            upper=max(upper,mp.log(rate)-rate*L-S*R)
        return upper

    for _ in range(100):
        sides=[one_side(y),one_side([-x for x in y])]
        if max(small,*sides)+PAD < W:
            return dict(cutoff=A,upper=W,AL=AL,delta=delta,
                        eps=eps,peak_cap=M)
        A*=2
    raise ArithmeticError("large-power tail failed to close")


def fixed_power_face_cases():
    # The Feller–Pareto archive subtracts its held minimum before this chart.
    archive=HERE.parent/"2026-11-07-fit-audit-data/cases.json.gz"
    with gzip.open(archive,"rt") as source:
        rows=json.load(source)
    result={}
    for case in rows:
        if case["index"] not in {1625,1628}:
            continue
        held={k:mp.mpf(v) for k,v in case["held"].items()}
        logs=[mp.log(mp.mpf(v)-held.get("min",0)) for v in case["data"]]
        y,s=standardized(logs)
        A=s*held["shape2"]
        R=max(y)-min(y)
        third=mean([x**3 for x in y])
        W=-mp.mpf("1.5")
        hL=2*mp.pi*mp.exp(2*W)
        u=A*R
        C=4*mp.exp(2*u)+mp.exp(3*u)
        error=A*A*(R**4*(mp.exp(u)/2+C/12)+1/(4*hL**2))
        leading=A*third/3
        assert u<1 and abs(leading)>error+PAD
        r=1 if leading>0 else 0
        sign=1 if r==0 else -1
        K=mp.log(mean([mp.exp(sign*A*x) for x in y]))
        kl,k,kh=shape(K)
        F=mp.log(A)+k*(mp.log(k)-1-K)-mp.loggamma(k)
        assert F>W+PAD
        result[str(case["index"]).zfill(6)]=dict(power=A,skew=third,curvature_floor=hL,
               leading=leading,error=error,winner="trgamma" if r==0 else "invtrgamma",
               standardized_value=F,shape=k,logLik=len(y)*(F-mp.log(s))-mp.fsum(logs),
               boxes=1)
    assert len(result)==2
    return result


def local_ball(y,initial):
    """Inclusion and strict negative curvature for a four-coefficient ball."""
    def score(A,c,p,q):
        t=[A*x-c for x in y]
        w=[1/(1+mp.exp(-v)) for v in t]
        v=[p-(p+q)*b for b in w]
        common=mp.digamma(p+q)
        return (1/A+mean([x*b for x,b in zip(y,v)]),-mean(v),
                common-mp.digamma(p)-mean([mp.log1p(mp.exp(-x)) for x in t]),
                common-mp.digamma(q)-mean([mp.log1p(mp.exp(x)) for x in t]))
    root=list(mp.findroot(score,initial,tol=mp.mpf("1e-57")))
    radius=mp.mpf("1e-10")
    # Trigamma is decreasing: enclose each positive term separately.
    mp.iv.dps=50
    I=mp.iv.mpf
    interval=lambda l,r:I([mp.nstr(l,60),mp.nstr(r,60)])
    A,c,p,q=[interval(x-radius,x+radius) for x in root]
    def trig(a,b):
        return interval(mp.polygamma(1,b),mp.polygamma(1,a))
    pp,qq=root[2:]
    t=trig(pp+qq-2*radius,pp+qq+2*radius)
    tp,tq=trig(pp-radius,pp+radius),trig(qq-radius,qq+radius)
    z=[I(mp.nstr(v,60)) for v in y]
    w=[1/(1+mp.iv.exp(c-A*x)) for x in z]
    def avg(xs):
        return sum(xs)/len(xs)
    v=[b*(1-b) for b in w]
    e=(p+q)*avg([b*x for x,b in zip(z,v)])
    B=[[(1/A)**2+(p+q)*avg([b*x*x for x,b in zip(z,v)]),-e,
         -avg([x*(1-b) for x,b in zip(z,w)]),avg([x*b for x,b in zip(z,w)])],
        [-e,(p+q)*avg(v),avg([1-b for b in w]),-avg(w)],
        [0,0,tp-t,-t],
        [0,0,-t,tq-t]]
    for i in range(4):
        for j in range(i):
            B[i][j]=B[j][i]
    trace=sum(B[i][i] for i in range(4))
    determinant=I(1)
    for k in range(4):
        d=B[k][k]
        assert d>0
        determinant*=d
        for i in range(k+1,4):
            for j in range(k+1,4):
                B[i][j]-=B[i][k]*B[k][j]/d
    curvature=determinant/trace**3
    assert curvature > I("1e-9")
    resid=mp.sqrt(mp.fsum(v*v for v in score(*root)))
    assert resid < radius*mp.mpf("1e-10")
    return dict(root=root,radius=radius,residual=resid,curvature_lower=str(curvature.a))


def run():
    mp.mp.dps=65
    source=golden("gb2")
    y,s=standardized([mp.log(mp.mpf(x)) for x in source["data"]])
    ep={k:mp.mpf(v) for k,v in source["estimates"].items()}
    p,q=ep["shape3"],ep["shape1"]
    a=ep["shape2"]*s
    b=(mp.log(ep["scale"])-mean([mp.log(mp.mpf(x)) for x in source["data"]]))/s
    ratio=p/(p+q)
    mode=b+mp.log(p/q)/a
    k=p*q/(p+q)
    W=mp.log(a)+height(k,ratio)[0]-k*mean([kernel(a*(x-mode),ratio)[0] for x in y])-PAD
    level=W-mp.mpf("0.00001")
    T,normal_upper=normal_tail(y,level)
    tail=large_tail(y,level)
    H=tail["cutoff"]
    print("GB2 exterior bounds passed",mp.nstr(T,6),mp.nstr(H,6),flush=True)
    normal=-mp.log(2*mp.pi*mp.e)/2
    face_target=level-normal
    faces={"trgamma":fit_side(y,face_target),
           "invtrgamma":fit_side([-x for x in y],face_target)}
    face_margin=W-level
    # A conservative comparison for each entire ratio-end slab on [T,H].
    rho=face_margin*mp.exp(-H*(max(y)-min(y)))/16
    assert 2*rho+2*mp.log1p(rho*mp.expm1(H*(max(y)-min(y))))<face_margin
    print("GB2 golden excludes both gamma faces",flush=True)
    small_box=[mp.mpf(s) for s in (".249",".251",".498",".502")]
    upper=value_bound(y,*small_box,level)
    assert upper<W
    local=local_ball(y,(a,a*b,p,q))
    queue=[]
    grid=np.array([float(u) for u in y])
    serial=count()
    def enqueue(lo,hi,l,r):
        upper=value_bound(y,lo,hi,l,r,level,grid)
        if upper>=W:
            heapq.heappush(queue,(-upper,next(serial),lo,hi,l,r))

    seed=[T,*[mp.mpf(x) for x in (".25",".5","1","2","4","8","16","32","64","128")
              if T<mp.mpf(x)<H],H]
    for lo,hi in zip(seed,seed[1:]):
        enqueue(lo,hi,mp.mpf(0),mp.mpf(1))
    examined=len(seed)-1
    for _ in range(384):
        if not queue:
            break
        _,_,lo,hi,l,r=heapq.heappop(queue)
        if min(mp.log(hi/lo),(hi-lo)*(max(y)-min(y)))>3*(r-l) and (lo<32 or r-l<mp.mpf('.04')):
            mid=mp.sqrt(lo*hi)
            enqueue(lo,mid,l,r)
            enqueue(mid,hi,l,r)
        else:
            mid=(l+r)/2
            enqueue(lo,hi,l,mid)
            enqueue(lo,hi,mid,r)
        examined+=2
    result={"precision":mp.mp.dps,"fixed_power_archive":fixed_power_face_cases(),
            "golden":{"power":a,"ratio":ratio,"witness":W,"seam":T,
                      "seam_upper":normal_upper,"large_tail":tail,"gamma_faces":faces,
                      "face_slab_log10":mp.log10(rho),"excluded_example":{"box":small_box,"upper":upper},
                      "local_ball":local,
                      "cells":examined,"unexcluded":len(queue),
                      "best_unexcluded_upper":-queue[0][0] if queue else None}}
    t=[mp.mpf(-1),-mp.mpf(1),mp.mpf(0),mp.mpf(1),mp.mpf(1)]
    v=mean([x*x for x in t])
    m4=mean([x**4 for x in t])
    c0=(m4-3*v*v)/(48*v)
    c1=-(m4-v*v)/(24*v)
    NF=-mp.log(2*mp.pi*mp.e*v)/2
    AL=asymmetric_laplace(t)
    assert max(c0,c1)<0 and AL>NF
    result["strict_trough"]=dict(log_sample=t,quadratic_at_ratio_half=c0,
                                 quadratic_at_ratio_endpoint=c1,normal=NF,AL=AL)
    (HERE/"gb2-results.json").write_text(json.dumps(result,indent=2,default=lambda x:mp.nstr(x,28))+"\n")
    print("GB2 archive comparisons and bounded-region experiment completed")


if __name__=="__main__":
    run()
