"""80-digit likelihood replays and interval checks for fixed profile knots."""
import json
from pathlib import Path

import mpmath as mp

mp.mp.dps = 80
mp.iv.dps = 65
M = mp.mpf
IV = mp.iv.mpf
ROOT = Path(__file__).resolve().parents[4]
DIST = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'


def terms(k, c, b, y, w, ctx=mp):
    z = [c*v+b for v in y]
    p = [1/(1+ctx.exp(-v)) for v in z]
    h = [ctx.log1p(ctx.exp(v)) for v in z]
    avg = lambda values: sum(a*v for a,v in zip(w,values))
    value = ctx.log(k*c)+avg(v-(1+k)*t for v,t in zip(z,h))
    score = [
        1/k-avg(h),
        1/c+avg(v*(1-(1+k)*a) for v,a in zip(y,p)),
        1-(1+k)*avg(p),
    ]
    q = [a*(1-a) for a in p]
    v = [-avg(a*t for a,t in zip(y,p)), -avg(p)]
    hcc = -1/c**2-(1+k)*avg(a*a*t for a,t in zip(y,q))
    hcb = -(1+k)*avg(a*t for a,t in zip(y,q))
    hbb = -(1+k)*avg(q)
    hess = [[-1/k**2, *v], [v[0], hcc, hcb], [v[1], hcb, hbb]]
    return value, score, hess


def bisect(fun, lo, hi, steps=75):
    assert fun(lo) >= 0 and fun(hi) <= 0, (lo,hi)
    for _ in range(steps):
        mid = (lo+hi)/2
        if fun(mid)>0:
            lo=mid
        else:
            hi=mid
    return (lo+hi)/2


def fixed(y,w,k,guess=None):
    if guess is None:
        # Binary64 proposals do not enter interval comparisons.
        import numpy as np
        from search import burr
        center, width = min(y), max(y)-min(y)
        y0 = np.array([float((v-center)/width) for v in y])
        _, _, (c0,b0) = burr(y0[None,:], np.array([[float(a) for a in w]]), np.array([float(k)]))
        c = M(float(c0[0,0]))/width
        guess = c, M(float(b0[0,0]))-c*center
    def func(c,b):
        return terms(k,c,b,y,w)[1][1:]
    def jac(c,b):
        return mp.matrix([row[1:] for row in terms(k,c,b,y,w)[2][1:]])
    return tuple(mp.findroot(func, guess, J=jac, tol=M('1e-68')))


def joint(y,w,guess):
    fun = lambda k,c,b: terms(k,c,b,y,w)[1]
    jac = lambda k,c,b: mp.matrix(terms(k,c,b,y,w)[2])
    fit = list(mp.findroot(fun,guess,J=jac,tol=M('1e-68'),maxsteps=35))
    val, score, hess = terms(*fit,y,w)
    cross = mp.matrix(hess[0][1:])
    lower = mp.matrix([row[1:] for row in hess[1:]])
    curvature = hess[0][0]-(cross.T*(lower**-1)*cross)[0]
    assert max(map(abs,score)) < M('1e-33')
    return fit, val, curvature


def weibull(y,w):
    def at(c):
        e = [mp.exp(c*t) for t in y]
        total = sum(a*t for a,t in zip(w,e))
        mean = sum(a*t for a,t in zip(w,y))
        g = 1/c+mean-sum(a*t*u for a,t,u in zip(w,y,e))/total
        return g,mp.log(c)+c*mean-mp.log(total)-1,[t/total for t in e]
    hi = M(1)/(max(y)-min(y))
    while at(hi)[0] > 0:
        hi *= 2
    c = mp.findroot(lambda c: at(c)[0],(hi/2,hi),tol=M('1e-70'))
    _,val,v = at(c)
    return c,val,sum(a*t*t for a,t in zip(w,v))/2-1


def pareto(y,w):
    return -mp.log(sum(a*(v-min(y)) for a,v in zip(w,y)))-1


def lomax(y,w,t,ctx=mp):
    z = [v+t for v in y]
    q = [1/(1+ctx.exp(v)) for v in z]
    G = sum(a*ctx.log1p(ctx.exp(v)) for a,v in zip(w,z))
    Q = sum(a*v for a,v in zip(w,q))
    # Unit-mean normalization is unnecessary for rate or gap evaluation.
    score = G*Q-(1-Q)
    gap = t-ctx.log(G)-G+ctx.log(sum(a*ctx.exp(v) for a,v in zip(w,y)))
    return score, gap, 1/G


def i_number(x):
    """Enclose the exact dyadic represented by an mpf."""
    return IV(x)


def box(lo, hi):
    return IV([lo,hi])


def contained(y,w, k_band, center, radii, joint_box=False):
    """Verify preconditioned inclusion; exact dyadic proposal centers/radii."""
    n = len(center)
    offset = 0 if joint_box else 1
    k0 = (k_band.a+k_band.b)/2
    midpoint = lambda v: M(((v.a+v.b)/2)._mpi_[0])
    middle = center if joint_box else [midpoint(k0),*center]
    data = [midpoint(v) for v in y]
    weights = [midpoint(v) for v in w]
    h = terms(*middle,data,weights)[2]
    inverse = -mp.matrix([row[offset:] for row in h[offset:]])**-1
    preconditioner = [[i_number(inverse[i,j]) for j in range(n)] for i in range(n)]
    def coords(bounded):
        pts = [i_number(v)+box(-r,r) if bounded else i_number(v)
               for v,r in zip(center,radii)]
        return pts if joint_box else [k_band,*pts]
    _, g, _ = terms(*coords(False),y,w,mp.iv)
    _, _, h = terms(*coords(True),y,w,mp.iv)
    for i,r in enumerate(radii):
        residual = abs(sum(preconditioner[i][j]*g[offset+j] for j in range(n)))
        for l,s in enumerate(radii):
            entry = (i==l)+sum(preconditioner[i][j]*h[offset+j][offset+l] for j in range(n))
            residual += abs(entry)*s
        assert residual.b < r, (i,residual,r,center,k_band)
    return coords(True),h


def determinant(h):
    if len(h)==2:
        return h[0][0]*h[1][1]-h[0][1]*h[1][0]
    return sum((-1)**i*h[0][i]*determinant([[row[j] for j in range(3) if j!=i] for row in h[1:]]) for i in range(3))


def negative_definite(h):
    a=[[-v for v in row] for row in h]
    assert a[0][0].a > 0
    assert determinant([row[:2] for row in a[:2]]).a > 0
    assert determinant(a).a > 0


def fmt(*args):
    return ' '.join(mp.nstr(a,20) for a in args)


def golden(name):
    raw = json.loads((DIST/name/'golden.json').read_text())
    x = [M(float(v)) for v in raw['data']]
    sign = 1 if name == 'burr' else -1
    y = [sign*mp.log(v) for v in x]
    w = [M(1)/len(y)]*len(y)
    est = raw['estimates']
    k,c,s = [M(est[key]) for key in ['shape1','shape2','scale']]
    fit,val,curv = joint(y,w,(k,c,-c*sign*mp.log(s)))
    k,c,b = fit
    face_w = weibull(y,w)
    p = pareto(y,w)
    restored_scale = mp.exp(-sign*b/c)
    full = len(y)*val-sum(mp.log(v) for v in x)
    assert abs(full-raw['logLik'])<M('1e-9')
    assert all(abs(a/est[key]-1)<M('1e-6') for a,key in zip([k,c,restored_scale],est))
    assert curv < 0 and val>max(p,face_w[1])
    print(name, 'joint',fmt(k,c,restored_scale,full,(val-p)*len(y),(val-face_w[1])*len(y),curv))
    held_c = M(2)
    lograte = mp.findroot(lambda t: lomax([held_c*v for v in y],w,t)[0],(M(-1),M(0)),tol=M('1e-60'))
    _,gap, held_k = lomax([held_c*v for v in y],w,lograte)
    assert gap > 0
    c1,b1 = fixed(y,w,M(1),(c,b))
    def held_scale(power):
        h = [mp.log1p(mp.exp(power*v)) for v in y]
        p = [1/(1+mp.exp(-power*v)) for v in y]
        G = sum(a*t for a,t in zip(w,h))
        return terms(1/G,power,0,y,w)[1][1],1/G
    c2 = mp.findroot(lambda power: held_scale(power)[0],(M(1),M(3)),tol=M('1e-60'))
    assert c2>0 and abs(held_scale(c2)[0])<M('1e-50')
    direct = [c*(sign*mp.log(v)-sign*mp.log(restored_scale)) for v in x]
    fd = sum(mp.log(k*c/v)+z-(1+k)*mp.log1p(mp.exp(z)) for v,z in zip(x,direct))
    assert abs(fd-full)<M('1e-66')
    # Exact rational shifts and reciprocals map the same dyadic original
    # inputs. Round neither transformed observation before taking logs.
    shifted_x = [v+4 for v in x]
    translated = [mp.log(v-4)*sign for v in shifted_x]
    assert max(abs(v-t) for v,t in zip(y,translated)) < M('1e-65')
    print(name, 'held k=1 c and Burr scale; held s=1 k and c; held c=2 k, log rate and E gap',
          fmt(c1,mp.exp(-b1/c1),held_scale(c2)[1],c2,held_k,lograte,gap))
    print(name, 'face gaps P-W, seam A',fmt((p-face_w[1])*len(y),face_w[2]))


def transition():
    # Prior finite/Pareto transition family, re-solve local branch equality.
    def data(q):
        return [0,M('0.23826')*q,7*q/15,8*q/15,8*q/15,1,1]
    w = [M(1)/7]*7
    def system(q,k,c,b):
        y = data(q)
        value,g,_ = terms(k,c,b,y,w)
        return [*g, value-pareto(y,w)]
    q,k,c,b = mp.findroot(system,(M('.95'),M(1),M(5),M(-3)),tol=M('1e-68'),maxsteps=100)
    assert 0<q<1
    for dq in [-M('1e-8'),0,M('1e-8')]:
        y = data(q+dq)
        fit,val,curv = joint(y,w,(k,c,b))
        P,W = pareto(y,w),weibull(y,w)[1]
        assert P>W and curv<0
        assert abs(val-P)<M('1e-63') if not dq else (val-P)*dq>0
        print('Pareto transition branch',fmt(q+dq,*fit,val-P,W-P))
    def l_gap(t,r):
        return lomax([0,mp.log(r)],[M('.5')]*2,t)
    t,r = mp.findroot(lambda t,r: l_gap(t,r)[:2],(M(-1),M(29)),tol=M('1e-68'))
    assert 28<r<30
    for dr in [-M('1e-8'),0,M('1e-8')]:
        rt = r+dr
        tt = mp.findroot(lambda u:l_gap(u,rt)[0],(t-M('.01'),t+M('.01')),tol=M('1e-68'))
        _,gap,k = l_gap(tt,rt)
        assert abs(gap)<M('1e-63') if not dr else gap*dr>0
        print('held-power transition branch',fmt(rt,tt,k,gap))
    w = [M('.5')]*2
    for eps in [-M('1e-12'),0,M('1e-12')]:
        z = [eps,1]
        if eps<0:
            def profile(c):
                G=sum(a*mp.log1p(mp.exp(c*v)) for a,v in zip(w,z))
                k=1/G
                value=terms(k,c,0,z,w)[0]
                return terms(k,c,0,z,w)[1][1],k,value
            u=bisect(lambda u:profile(u/-eps)[0],M('1e-10'),M(2),170)
            assert u>0
            print('held scale negative-side',fmt(eps,u/-eps,*profile(u/-eps)[1:]))
        else:
            Z=sum(v==0 for v in z)
            P=-mp.log(sum(a*v for a,v in zip(w,z)))-1-Z*mp.log(2)/2
            c=M('1e16') if not eps else 100/eps
            k=1/sum(a*mp.log1p(mp.exp(c*v)) for a,v in zip(w,z))
            val=terms(k,c,0,z,w)[0]
            assert 0<P-val<M('1e-14')
            print('held scale Pareto-side',fmt(eps,P,P-val))
    for c in [M('.5'),M(1),M(2)]:
        for logeps in [-10,-30,-60]:
            eps = mp.exp(logeps)
            y = [mp.log(eps),mp.log(1+eps),mp.log(3+eps)]
            val=terms(M(2),c,M(0),y,[M(1)/3]*3)[0]*3-sum(y)
            print('shift spike',fmt(c,logeps,val,val-(c-1)*logeps))


def extra_transitions():
    w=[M(1)/7]*7
    base=[M(i)/15 for i in [0,5,7,8,8,15,15]]
    alt=[M(i) for i in [0,0,0,0,0,1,1]]
    def mix(t):
        return [(1-t)*a+t*b for a,b in zip(base,alt)]
    def seam(y):
        c,W,A=weibull(y,w)
        v=[mp.exp(c*t) for t in y]
        v=[t/sum(a*b for a,b in zip(w,v)) for t in v]
        d=mp.matrix([sum(wi*u*(u-1)*r for wi,u,r in zip(w,v,y)),sum(wi*u*(u-1) for wi,u in zip(w,v))])
        cy=sum(wi*u*r for wi,u,r in zip(w,v,y))
        J=mp.matrix([[1/c**2+sum(wi*u*r*r for wi,u,r in zip(w,v,y)),cy],[cy,1]])
        C=sum(wi*(u*u/2-u**3/3) for wi,u in zip(w,v))+(d.T*(J**-1)*d)[0]/2
        return c,W,A,C
    az=mp.findroot(lambda t:seam(mix(t))[2],(M('.0439'),M('.044')))
    for shift in [-M('1e-9'),M(0),M('1e-9')]:
        y=mix(az+shift)
        c,W,A,C=seam(y)
        assert W>pareto(y,w) and C>0
        k=M(10)**6
        vscale=sum(a*mp.exp(c*b) for a,b in zip(w,y))
        ob=-mp.log(k*vscale)
        cp,bp=fixed(y,w,k,(c,ob))
        v=terms(k,cp,bp,y,w)[0]
        assert abs(k*k*(v-W-A/k)-C)<M('1e-5')
        print('near zero Weibull slope',fmt(az+shift,A,C,k*k*(v-W-A/k)))
    def faces(t):
        y=[0,t,M(7)/15,M(8)/15,M(8)/15,1,1]
        p=pareto(y,w)
        W=weibull(y,w)[1]
        value=terms(M(1),M('4.932'),M('-2.624'),y,w)[0]
        return W-p,value-max(W,p)
    cross=mp.findroot(lambda t:faces(t)[0],(M('.23826'),M('.23827')))
    for shift in [-M('1e-8'),M(0),M('1e-8')]:
        delta,gap=faces(cross+shift)
        assert gap>M('.026')/7
        print('crossing of the two faces',fmt(cross+shift,7*delta,7*gap))
    # Free location at minus infinity with growing power. Density on the
    # original x scale tends to a generalized logistic density.
    x=[M(0),M(1),M(3)]
    k,a,d=M(2),M('.5'),M('1.25')
    limit=sum(mp.log(k/d)+(v-a)/d-(k+1)*mp.log1p(mp.exp((v-a)/d)) for v in x)
    for T in [M(1000),M(10)**6,M(10)**12]:
        s,c=T+a,T/d
        y=[mp.log(T+v) for v in x]
        finite=3*terms(k,c,-c*mp.log(s),y,[M(1)/3]*3)[0]-sum(y)
        assert abs(finite-limit)<10/T
        print('translation at minus infinity',fmt(T,finite,limit,finite-limit))
    for t in [M(10),M(30),M(60)]:
        # With c>=1 held and k and s free, the second free-threshold path.
        c,k,s=M(2),1/t,mp.exp(-t)
        y=[mp.log(v+s) for v in x]
        finite=3*terms(k,c,-c*mp.log(s),y,[M(1)/3]*3)[0]-sum(y)
        print('free location shrink with held c=2',fmt(t,finite))
        if t==60:
            assert finite>40


def weibull_upper(yi, wi, c0):
    def at(c):
        e=[mp.iv.exp(c*t) for t in yi]
        v=sum(a*b for a,b in zip(wi,e))
        mean=sum(a*b for a,b in zip(wi,yi))
        score=1/c+mean-sum(a*b*t for a,b,t in zip(wi,e,yi))/v
        return score,mp.iv.log(c)+c*mean-mp.iv.log(v)-1
    low,high=IV(c0)-IV('1e-12'),IV(c0)+IV('1e-12')
    gl,vl=at(low)
    gh,_=at(high)
    assert gl.a>0 and gh.b<0
    return (vl+gl*(high-low)).b


def pareto_cover(y,w,yi,wi,cw):
    """Use the existing dual/chord proof with outward scalar comparisons."""
    P=-mp.iv.log(sum(a*b for a,b in zip(wi,yi)))-1
    K = M(128)
    tail=weibull_upper(yi,wi,cw)+6*(IV(K)+1)/(2*(IV(K)+1-6)**2)
    assert tail.b<P.a
    cache={}
    def upper(k):
        if k not in cache:
            c,b=fixed(y,w,k)
            p=[IV(1/(1+mp.exp(-c*t-b))) for t in y]
            j=max(range(len(p)),key=lambda j: float((p[j]*(1-p[j])).a))
            p[j]=6/(1+IV(k))-sum(v for i,v in enumerate(p) if i!=j)
            assert all(v.a>0 and v.b<1 for v in p)
            A=sum(a*((1+IV(k))*v-1)*t for a,v,t in zip(wi,p,yi))
            assert A.a>0
            entropy=-sum(a*(v*mp.iv.log(v)+(1-v)*mp.iv.log(1-v)) for a,v in zip(wi,p))
            cache[k]=(mp.iv.log(IV(k)/A)-1-(1+IV(k))*entropy).b
            assert len(cache)<2000
        return cache[k]
    stack=[(M(2)**i,M(2)**(i+1)) for i in range(-3,7)]
    count=0
    while stack:
        lo,hi=stack.pop()
        excess=(IV(hi)-lo)**2/(8*IV(lo)**2)
        if (max(upper(lo),upper(hi))+excess).b<P.a:
            count+=1
            continue
        mid=(lo+hi)/2
        stack.extend([(lo,mid),(mid,hi)])
    print('strict Pareto dominance on double-mode sample, duals and cells',len(cache),count)


def competing():
    y=[M(i)/8 for i in [0,2,2,5,6,8]]
    w=[M(1)/6]*6
    yi=[IV(i)/8 for i in [0,2,2,5,6,8]]
    wi=[IV(1)/6]*6
    p,wb=pareto(y,w),weibull(y,w)
    W_upper=weibull_upper(yi,wi,wb[0])
    for ka,kb in [('.21','.25'),('6.72','8.01')]:
        def score(k):
            c,b=fixed(y,w,k)
            return terms(k,c,b,y,w)[1][0]
        k0=bisect(score,M(ka),M(kb),25)
        c,b=fixed(y,w,k0)
        fit,val,curv=joint(y,w,(k0,c,b))
        radii=[M('1e-9')]*3
        pts,h=contained(yi,wi,IV(fit[0]),fit,radii,joint_box=True)
        negative_definite(h)
        assert curv < 0
        enclosure=terms(*pts,yi,wi,mp.iv)[0]
        assert enclosure.a>W_upper
        print('Burr certified local mode',fmt(*fit,val-p,val-wb[1],curv),
              'root k box',pts[0],'value box',enclosure)
    print('Burr double-mode sample: P-W, A',fmt(p-wb[1],wb[2]))
    pareto_cover(y,w,yi,wi,wb[0])
    for logs, brackets in [([0,5,6,8],[(-4.25,-4),(-1.5,-1.25)])]:
        y=list(map(M,logs))
        w=[M(1)/len(y)]*len(y)
        wi=[IV(1)/len(y)]*len(y)
        yi=list(map(IV,logs))
        for a,b in brackets:
            ai,bi=IV(a),IV(b)
            left=lomax(yi,wi,ai,mp.iv)[0]
            right=lomax(yi,wi,bi,mp.iv)[0]
            assert left.a>0 and right.b<0, (logs,a,b,left,right)
            t=bisect(lambda u:lomax(y,w,u)[0],M(a),M(b),200)
            _,gap,k=lomax(y,w,t)
            print('held power certified sign band',logs,(a,b),fmt(t,k,gap),
                  'endpoint scores',fmt(lomax(y,w,M(a))[0],lomax(y,w,M(b))[0]))


if __name__ == '__main__':
    competing()
    for name in ['burr','invburr']:
        golden(name)
    transition()
    extra_transitions()
