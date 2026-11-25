"""Signed transformed-gamma profile bounds, in centered unit-variance log data."""
import json
from functools import cache
from pathlib import Path

import mpmath as mp

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]


def golden(family):
    return json.loads((ROOT / "univariateML-cpp/include/cppunivariate/distributions"
                       / family / "golden.json").read_text())


def shape(gap):
    # Damped Newton proposes. Monotonicity and signed endpoints provide inclusion.
    f = lambda k: mp.log(k) - mp.digamma(k) - gap
    k = mp.mpf("0.7") / gap
    for _ in range(100):
        step = f(k) / (1/k-mp.polygamma(1,k))
        if abs(step) < k*mp.mpf("1e-42"):
            error = k*mp.mpf("1e-38")
            assert f(k-error) > 0 > f(k+error)
            return k-error, k, k+error
        k = max(k/2, k-step)
    raise ArithmeticError("shape proposal failed")


def fit_side(z, witness, slab=None, start=None, held_mode=False):
    M = max(z)
    n = len(z)
    pad = mp.mpf("1e-28")

    @cache
    def point(t):
        weights=[mp.exp(t*u) for u in z]
        if held_mode:
            K=mp.fsum(mp.expm1(t*u)-t*u for u in z)/n
            slope=mp.fsum(u*mp.expm1(t*u) for u in z)/n
            var=mp.fsum(u*u*w for u,w in zip(z,weights))/n
        else:
            total=mp.fsum(weights)
            K=mp.log(total/n)
            slope=mp.fsum(u*w for u,w in zip(z,weights))/total
            var=mp.fsum((u-slope)**2*w for u,w in zip(z,weights))/total
        kl,k,kh = shape(K)
        value = mp.log(t)+k*(mp.log(k)-1-K)-mp.loggamma(k)+mp.log(2*mp.pi*mp.e)/2
        return K,slope,var,kl,k,kh,value

    T=mp.mpf(".0625") if start is None else start
    if start is None:
        for _ in range(100):
            A,C=seam_coefficients(z,T,held_mode)
            xs=[0,T]
            if C<0 and 0 < -A/(2*C) < T:
                xs.append(-A/(2*C))
            if max(A*x+C*x*x for x in xs)+pad < witness:
                break
            T/=2
        else:
            raise ArithmeticError("seam comparison failed")
    norm=mp.log(2*mp.pi*mp.e)/2
    if not held_mode:
        cutoff=4*(n+mp.log(n)+10)/M
        face=norm-1-mp.log(M)
    elif M<=0:
        gap=-mp.fsum(z)/n
        cutoff=(2+mp.exp(mp.euler))/gap
        face=norm-1-mp.log(gap)
    else:
        face=-mp.inf
        cutoff=max(1,1/M)
        while norm-1+mp.log(n*cutoff/(mp.expm1(cutoff*M)-cutoff*M))>=witness:
            cutoff*=2
    if face > witness+pad:
        raise ArithmeticError("large-power face refutes witness")

    initial = [T,cutoff] if slab is None else [T,*slab,cutoff]
    assert all(l<r for l,r in zip(initial,initial[1:]))
    stack=[(l,r,slab is not None and (l,r)==slab) for l,r in zip(initial,initial[1:])]
    counts=dict(cells=0,curvature=0,monotone=0,value=0)
    while stack:
        l,r,curvature_only=stack.pop()
        counts["cells"]+=1
        if counts["cells"] > 100_000:
            raise ArithmeticError("cell allowance exhausted")
        L,P,V,kl,k,kh,F=point(l)
        R,Q,_,rl,_,rh,G=point(r)
        # Endpoint tangents to log(t)-k_sec*K(t), with a residual allowance
        # supplied by the signed shape brackets and concavity in k.
        # The endpoint likelihood upper adds 1e-28 above the tangent residual.
        span=max(map(abs,z)) if held_mode else M-min(z)
        upper=-1/r**2+Q**2/(mp.polygamma(1,kh)-1/kh)
        upper-=rl*V*mp.exp(-(r-l)*span)
        if curvature_only:
            if upper < -pad:
                counts["curvature"]+=1
                continue
        else:
            if (1/l-rl*P < -pad or 1/r-kh*Q > pad
                    or (upper < -pad and (1/l-kl*P < -pad or 1/r-rh*Q > pad))):
                counts["monotone"]+=1
                continue
            if (r-l)*span<1:
                lower=-1/l**2+P**2/(mp.polygamma(1,rl)-1/rl)
                lower-=kh*V*mp.exp((r-l)*span)
                t=(l+r)/2
                _,p,_,klm,_,khm,_=point(t)
                if (1/t-khm*p+min(0,-upper*(t-l),lower*(r-t))>pad
                        or 1/t-klm*p+max(0,-lower*(t-l),upper*(r-t))<-pad):
                    counts["monotone"]+=1
                    continue
            sec=(mp.log(r/l)+F-G)/(R-L)
            # Convexity of the conditional conjugate gives the chord.
            # Use endpoint value uppers with the same allowance at each end.
            dleft=1/l-sec*P
            dright=1/r-sec*Q
            checks=[l,r]
            if dleft>dright:
                checks.append(min(r,max(l,l+(G-F-dright*(r-l))/(dleft-dright))))
            bound=max(min(F+dleft*(t-l),G+dright*(t-r)) for t in checks)
            if bound+pad < witness:
                counts["value"]+=1
                continue
        mid=(l+r)/2
        stack.extend(((l,mid,curvature_only),(mid,r,curvature_only)))
    if slab:
        l,r=slab
        _,P,_,_,_,kh,_=point(l)
        _,Q,_,rl,*_=point(r)
        assert 1/l-kh*P>pad and 1/r-rl*Q<-pad
    return dict(**counts,points=point.cache_info().currsize,cutoff=cutoff,seam=T,face=face)


def standardized(y):
    m=mp.fsum(y)/len(y)
    s=mp.sqrt(mp.fsum((u-m)**2 for u in y)/len(y))
    return [(u-m)/s for u in y],s


def proposal(z,t):
    n=len(z)
    w=[mp.exp(t*u) for u in z]
    total=mp.fsum(w)
    K=mp.log(total/n)
    kl,k,kh=shape(K)
    ratio=mp.fsum(u*v for u,v in zip(z,w))/total
    return (mp.log(t)+k*(mp.log(k)-1-K)-mp.loggamma(k)+mp.log(2*mp.pi*mp.e)/2,
            1/t-kh*ratio,1/t-kl*ratio)


def seam_coefficients(z,T,held_mode=False):
    B=max(map(abs,z))
    skew=mp.fsum(u**3 for u in z)/len(z)
    fourth=mp.fsum(u**4 for u in z)/len(z)
    h=T*B
    c=fourth*mp.exp(-h)/12
    if not held_mode:
        c-=mp.exp(2*h)/4
    d=1-abs(skew)*T/3-abs(c)*T*T
    Kmax=T*T*mp.exp(h)/2
    if d<=0 or Kmax>mp.mpf(".5"):
        return mp.mpf(0),mp.inf
    A=-skew/6
    C=-c/2+(abs(skew)/3+abs(c)*T)**2/(4*d*d)
    C-=d/(6*(1+mp.sqrt(1+4*Kmax/3)))
    C+=T**4*mp.exp(3*h)/360
    return A,C


def near_seam():
    # The analytical reconstruction of held-study.cpp, before rounding x to double.
    with mp.workdps(85):
        z0=[mp.mpf(-3)+mp.mpf(6)*i/80 for i in range(81)]
        sq=mp.fsum(x*x for x in z0)/81
        z,_=standardized([x-mp.mpf("1e-8")*(x*x-sq) for x in z0])
        T=mp.mpf("0.00003")
        outer=mp.mpf(".04")
        R=max(z)-min(z)
        skew=mp.fsum(x**3 for x in z)/len(z)
        fourth=mp.fsum(x**4 for x in z)/len(z)
        fourth_cumulant=fourth-3
        # Interval operations evaluate the explicit analytic curvature bounds.
        mp.iv.dps=65
        interval=lambda x,y:mp.iv.mpf([mp.nstr(x,75),mp.nstr(y,75)])
        t=interval(-T,T)
        E=mp.mpf("1.1")*R**5*T
        D2=interval(fourth_cumulant/6-E,fourth_cumulant/6+E)
        D1=interval(skew/3,skew/3)+t*D2
        spread=T*(abs(skew)/3+T*(abs(fourth_cumulant)/6+E))
        low,high=1-spread,1+spread
        q=interval(low-high**2*T*T/6,high)
        smax=T*T*high
        alpha=interval(mp.mpf(1)/12-smax**2/120,mp.mpf(1)/12)
        alpha1=interval(-smax/60,0)
        qp=(D1-4*t*q*q*alpha-4*t**3*q**3*alpha1)/(1+4*t*t*q*alpha+2*t**4*q*q*alpha1)
        curvature=-D2/(2*q)+D1*qp/(2*q*q)-2*q*alpha-2*t*qp*alpha
        curvature-=2*t*q*alpha1*(2*t*q+t*t*qp)
        assert curvature.b < mp.iv.mpf("-0.01")
        f=lambda t:mp.fsum(proposal(z,t)[1:])/2
        # Bracket and compute the positive local root. Reflection gives the other sample.
        l,r=mp.mpf("1e-8"),mp.mpf("3e-7")
        assert proposal(z,l)[1]>0>proposal(z,r)[2]
        root=mp.findroot(f,(l,r),tol=mp.mpf("1e-50"))
        W=proposal(z,root)[0]-mp.mpf("1e-47")
        assert W>0
        sides={}
        for label,y in [("positive",z),("negative",[-u for u in z])]:
            A,C=seam_coefficients(y,outer)
            assert C<0 and A+2*C*T<0 and A*T+C*T*T<0
            sides[label]=fit_side(y,W,start=outer)
        return dict(precision=85,root=root,witness=W,curvature_interval=str(curvature),
                    central_radius=T,outer_radius=outer,**sides)


def exact_score_signs(decimal_logs, brackets):
    """Interval elementary functions; f(k) bounded by two Stirling terms."""
    mp.iv.dps=55
    I=mp.iv.mpf
    z=list(map(I,decimal_logs))
    n=len(z)
    em=sum(z)/n
    sd=mp.iv.sqrt(sum((x-em)**2 for x in z)/n)
    uz=[(v-em)/sd for v in z]
    approx,_=standardized(list(map(mp.mpf,decimal_logs)))
    pads=mp.mpf("0.000001")
    out=[]

    def f_bounds(x):
        shifted=x+64
        recurrence=mp.iv.ln(x/shifted)+sum(1/(x+j) for j in range(64))
        base=recurrence+1/(2*shifted)+1/(12*shifted**2)
        return base-1/(120*shifted**4),base

    for l,r in brackets:
        pair=[]
        for at,positive in [(l,True),(r,False)]:
            t=I(at)
            weights=[mp.iv.exp(t*x) for x in uz]
            total=sum(weights)
            K=mp.iv.ln(total/n)
            Kp=sum(x*w for x,w in zip(uz,weights))/total
            approx_t=mp.mpf(at)
            _,k,_=shape(mp.log(mp.fsum(mp.exp(approx_t*x) for x in approx)/n))
            kl=I(mp.nstr(k*(1-pads),40))
            kh=I(mp.nstr(k*(1+pads),40))
            assert f_bounds(kl)[0]>K and f_bounds(kh)[1]<K
            assert Kp>0
            lo,hi=1/t-kh*Kp,1/t-kl*Kp
            assert lo>0 if positive else hi<0
            pair.append([str(lo.a),str(hi.b)])
        out.append(pair)
    return out


def run():
    mp.mp.dps=60
    # The original decimal tokens define an exact sample x_i=exp(token_i).
    exact=json.loads((HERE.parent/"2026-11-07-trgamma-seam-data/multiplicity.json").read_text(),
                     parse_float=str)["candidates"][0]["logs"]
    z,_=standardized(list(map(mp.mpf,exact)))
    brackets=[("0.64","0.69"),("11.9","12.3")]
    signs=[[proposal(z,mp.mpf(t))[1:] for t in pair] for pair in brackets]
    assert all(left[0]>0>right[1] for left,right in signs)
    result={"precision":mp.mp.dps,"multimodal":{"logs":exact,"brackets":brackets,"scores":signs,
            "elementary_interval_score_bounds":exact_score_signs(exact,brackets)}}
    data=golden("gengamma")
    z,s=standardized([mp.log(mp.mpf(x)) for x in data["data"]])
    t=abs(mp.mpf(data["estimates"]["Q"]))*s/mp.mpf(data["estimates"]["sigma"])
    winner=[-u for u in z]
    slab=(t*mp.mpf(".995"),t*mp.mpf("1.005"))
    score=lambda t: mp.fsum(proposal(winner,t)[1:])/2
    root=mp.findroot(score,slab)
    witness=proposal(winner,t)[0]-mp.mpf("1e-31")
    result["golden"]={"witness":witness,"root":root,"slab":slab,
                      "negative":fit_side(winner,witness,slab),"positive":fit_side(z,witness)}
    z,_=standardized(list(map(mp.mpf,exact)))
    witness=mp.log(2*mp.pi*mp.e)/2-1-mp.log(min(max(z),-min(z)))
    result["archived_multimodal"]={"face_witness":witness,
                                  "positive":fit_side(z,witness),
                                  "negative":fit_side([-u for u in z],witness)}
    for name, x in [("power",[1,2,3]),("pareto",[1,.5,1/3]),("two_point",[1,2])]:
        z,_=standardized([mp.log(mp.mpf(v)) for v in x])
        witness=mp.log(2*mp.pi*mp.e)/2-1-mp.log(min(max(z),-min(z)))
        result[name]={"witness":witness,
                      "positive":fit_side(z,witness),
                      "negative":fit_side([-u for u in z],witness)}
    logs=[mp.log(x) for x in (1,2,3)]
    rms=mp.sqrt(mp.fsum(x*x for x in logs)/len(logs))
    z=[x/rms for x in logs]
    witness=mp.log(2*mp.pi*mp.e)/2-1-mp.log(mp.fsum(z)/len(z))
    result["held_mu_zero"]={"face_witness":witness,
        "positive":fit_side(z,witness,held_mode=True),
        "negative":fit_side([-x for x in z],witness,held_mode=True)}
    result["near_seam_reflections"]=near_seam()
    (HERE/"gamma-results.json").write_text(json.dumps(result,indent=2,default=lambda x:mp.nstr(x,28))+"\n")
    print("Signed gamma cover and exact-data score brackets passed")


if __name__=="__main__":
    run()
