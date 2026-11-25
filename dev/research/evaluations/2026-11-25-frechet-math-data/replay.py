"""High-precision replay, exact decimal witnesses and stored golden binary64 data."""
import json
from pathlib import Path

import mpmath as mp
import numpy as np
from mpmath import iv
from search import free, held, maxima

mp.mp.dps = 65
iv.dps = 50
M = mp.mpf
ROOT = Path(__file__).resolve().parents[4]
DIST = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'


def show(label, *values):
    print(label, *(mp.nstr(v, 16) for v in values), flush=True)


def mean(values, weights):
    return sum(p*v for p, v in zip(weights, values))


def free_point(x, p, b, y, ctx=mp, seam=False):
    z = [g*ctx.exp(-y) for g in x]
    t = z if seam else [ctx.ln(1+b*g)/b for g in z]
    r = z if seam else [g/(1+b*g) for g in z]
    db = [-g*g/2 for g in z] if seam else [(q-v)/b for q,v in zip(r,t)]
    w = [q*ctx.exp(-v) for q,v in zip(p,t)]
    total = sum(w)
    value = -y-(1+b)*mean(t,p)-ctx.ln(total)-1
    sy = -1+(1+b)*mean(r,p)-mean(r,w)/total
    sb = -mean(t,p)-(1+b)*mean(db,p)+mean(db,w)/total
    return value, sy, sb


def held_point(x, p, a, u, ctx=mp):
    e = ctx.exp(u/a)
    r = [e/(g+e) for g in x]
    logd = [ctx.ln(g+e) for g in x]
    h = [ctx.exp(-a*v) for v in logd]
    su = mean([q*(v-1-1/a) for q,v in zip(r,h)],p)
    sa = 1/a+mean([(v-1)*z for v,z in zip(h,logd)],p)-u*su/a
    value = ctx.ln(a)-mean([(1+a)*z+v for z,v in zip(logd,h)],p)
    return value, su, sa


def inner(fun, x, p, shape, seam=False):
    args = {'seam': True} if seam else {}
    score = lambda u: fun(x,p,shape,u,**args)[1]
    lo, hi = ((M(-40), M(40)) if fun is free_point else (mp.log(p[0]/2), M(0)))
    assert score(lo) > 0 and score(hi) < 0
    for _ in range(32):
        mid = (lo+hi)/2
        if score(mid)>0:
            lo = mid
        else:
            hi = mid
    root = mp.findroot(score, (lo,hi))
    assert abs(score(root)) < M('1e-58')
    return root


def verify_sign(fun, xs, counts, shape_text, sign):
    n = sum(counts)
    x, p = list(map(M,xs)), [M(k)/n for k in counts]
    shape = M(shape_text)
    seam = fun is free_point and shape == 0
    args = {'seam': True} if seam else {}
    root = inner(fun,x,p,shape,seam)
    ix, ip = list(map(iv.mpf,xs)), [iv.mpf(k)/n for k in counts]
    a = iv.mpf(shape_text)
    lo, hi = [mp.nstr(root+d*M('1e-24'), 40) for d in (-1,1)]
    assert fun(ix,ip,a,iv.mpf(lo),iv,**args)[1] > 0
    assert fun(ix,ip,a,iv.mpf(hi),iv,**args)[1] < 0
    score = fun(ix,ip,a,iv.mpf([lo,hi]),iv,**args)[2]
    assert sign*score > 0
    show(fun.__name__+' signed bracket',shape,fun(x,p,shape,root,**args)[2])


def scan(fun, x, p, label, hold=1):
    shapes = np.linspace(0,1,1001) if fun is free_point else np.geomspace(1, 1e5, 1001)
    x = [v-min(x) for v in x]
    d = max(x) if fun is free_point else M(hold)
    x = [v/d for v in x]
    search = free if fun is free_point else held
    k = maxima(search(np.array(x,dtype=float),np.array(p,dtype=float),shapes))
    for index in k:
        s = M(shapes[index])
        seam = fun is free_point and index==0
        u = inner(fun,x,p,s,seam)
        if 0<index<len(shapes)-1:
            u,s = mp.findroot(lambda u,s: fun(x,p,s,u)[1:], (u,s))
            assert max(map(abs,fun(x,p,s,u)[1:])) < M('1e-55')
        vals = fun(x,p,s,u,**({'seam': True} if seam else {}))
        show(label,s,u,vals[0]-mp.log(d),vals[2])


def seam_curvature(x,p,y):
    z=[v*mp.exp(-y) for v in x]
    w=[q*mp.exp(-v) for q,v in zip(p,z)]
    w=[q/sum(w) for q in w]
    z2=[v*v for v in z]
    z3=[v*v*v for v in z]
    z4=[v*v for v in z2]
    centered=lambda v: mean(v,p)-mean(v,w)
    varz=mean(z2,w)-mean(z,w)**2
    cov=mean(z3,w)-mean(z,w)*mean(z2,w)
    varz2=mean(z4,w)-mean(z2,w)**2
    yy=-1-varz
    yb=mean(z,p)-centered(z2)-cov/2
    bb=mean(z2,p)-2*centered(z3)/3-varz2/4
    return bb-yb*yb/yy


def two_point(p):
    q = 1-p
    t = mp.findroot(lambda t: 1/t-q+q*mp.exp(-t)/(p+q*mp.exp(-t)), (2,3))
    g = mp.log(t)-q*t-mp.log(p+q*mp.exp(-t))-1
    h = p*(1-2*p)/(mp.sqrt(p*q)+p)
    r = mp.ln((1+h)/h)
    f = mp.ln(mp.expm1(r))-2*q*r-mp.ln(p+q*mp.exp(-r))-1
    return f,g


def golden(name):
    return json.loads((DIST/name/'golden.json').read_text())


def fixed_frechet(x, a):
    x = [v-min(x) for v in x]
    def score(e):
        r = [e/(e+v) for v in x]
        return a*len(x)*sum(v**(a+1) for v in r)/sum(v**a for v in r)-(a+1)*sum(r)
    lo, hi = M(-60), M(60)
    for _ in range(60):
        mid = (lo+hi)/2
        if score(mp.exp(mid))>0:
            lo=mid
        else:
            hi=mid
    y = mp.findroot(lambda y: score(mp.exp(y)), (lo,hi))
    assert abs(score(mp.exp(y))) < M('1e-56')
    e = mp.exp(y)
    scale = (len(x)/sum((e+v)**(-a) for v in x))**(1/a)
    ell = len(x)*(mp.log(a)+a*mp.log(scale)-1)-(a+1)*sum(mp.log(e+v) for v in x)
    return e,scale,ell


def folded(x,p,mu):
    d = mean([(v-mu)**2 for v in x],p)
    if d==0:
        assert all(v==mu for v in x)
        return None
    c = [2*mu*v for v in x]
    def score(t):
        return t-d-2*mean([v/(1+mp.exp(v/t)) for v in c],p)
    hi = d+mean(c,p)
    if hi == d:
        return d
    assert score(d)<0<score(hi)
    lo=d
    for _ in range(220):
        mid=(lo+hi)/2
        if score(mid)>0:
            hi=mid
        else:
            lo=mid
    return (lo+hi)/2


def folded_location(x,p,t):
    if mean([v*v for v in x],p)<=t:
        return M(0)
    def score(mu):
        return mean([v*mp.tanh(v*mu/t) for v in x],p)/mu-1
    hi=mean(x,p)
    lo=M(0)
    for _ in range(220):
        mid=(lo+hi)/2
        if score(mid)>0:
            lo=mid
        else:
            hi=mid
    return (lo+hi)/2


def folded_joint(x,p,a):
    m2 = mean([v*v for v in x],p)
    t=2*m2/(1+mp.sqrt(1+4*m2*a*a))
    mu=a*t
    value= -mp.log(2*mp.pi*t)/2-(m2+mu**2)/(2*t)+mean(
        [mp.log(2*mp.cosh(a*v)) for v in x],p)
    score=mean([v*mp.tanh(a*v) for v in x],p)-mu
    return value,score,mu,t


if __name__ == '__main__':
    for b, sign in [('0',1),('.02',1),('.15',-1),('1',1)]:
        verify_sign(free_point, ['0','1','2.6'], [1,1,1], b, sign)
    for a, sign in [('1',-1),('1.35',1),('1.75',-1)]:
        verify_sign(held_point, ['0','.05','40'], [3,4,1], a, sign)
    for upper in ('2.55','2.6','2.7'):
        scan(free_point,[M(0),M(1),M(upper)],[M(1)/3]*3,'free '+upper)
    for upper in ('39','40'):
        scan(held_point,[M(0),M('.05'),M(upper)],[M(k)/8 for k in (3,4,1)],
             'held '+upper)
    p=[M(1)/3]*3
    def seam_score(y,r):
        return free_point([M(0),M(1),r],p,M(0),y,seam=True)[1:]
    y,r=mp.findroot(seam_score,(M('-.1'),M('2.56')))
    show('zero first seam score: sample r, profiled second derivative',
         r,seam_curvature([M(0),M(1),r],p,y))
    for offset in (-M('1e-8'),M('1e-8')):
        x=[M(0),M(1),r+offset]
        yy=inner(free_point,x,p,M(0),True)
        show('near seam r, inward score',r+offset,free_point(x,p,0,yy,seam=True)[2])

    p=[M(1)/3]*3
    def free_tie(y0,y1,b,r):
        x=[M(0),M(1),r]
        left=free_point(x,p,b,y0)
        right=free_point(x,p,M(1),y1)
        return left[1],right[1],left[2],right[0]-left[0]
    _,_,b,r=mp.findroot(free_tie,(M('-.28'),M('-1.56'),M('.12'),M('2.65')))
    assert M('2.6')<r<M('2.7') and 0<b<M('.65')
    show('free local branches tie r,b',r,b)

    p=[M(k)/8 for k in (3,4,1)]
    def held_tie(u0,u1,a,r):
        x=[M(0),M('.05'),r]
        left=held_point(x,p,M(1),u0)
        right=held_point(x,p,a,u1)
        return left[1],right[1],right[2],right[0]-left[0]
    _,_,a,r=mp.findroot(held_tie,(M('-.75'),M('-.55'),M('1.56'),M('39.45')))
    assert 39<r<40 and M('1.2')<a<10
    show('held local branches tie r,lambda',r,a)

    p=[M(k)/10 for k in (4,5,1)]
    def face_tie(y0,y1,r):
        x=[M(0),M(1),r]
        left=free_point(x,p,M(0),y0,seam=True)
        right=free_point(x,p,M(1),y1)
        return left[1],right[1],right[0]-left[0]
    y0,y1,r=mp.findroot(face_tie,(M('-.4'),M('-2.4'),M('1.46')))
    assert M('1.4')<r<M('1.5')
    show('finite/Gumbel equality r',r)
    for offset in (-M('1e-8'),M('1e-8')):
        x=[M(0),M(1),r+offset]
        g=free_point(x,p,0,inner(free_point,x,p,M(0),True),seam=True)[0]
        f=free_point(x,p,1,inner(free_point,x,p,M(1)))[0]
        show('finite/Gumbel nearby r, difference',r+offset,f-g)

    pc = mp.findroot(lambda p: two_point(p)[0]-two_point(p)[1], (M('.4'),M('.42')))
    for p in (M(1)/3,pc-M('1e-10'),pc,pc+M('1e-10'),M(49)/100):
        f,g = two_point(p)
        show('two-point fraction, finite-Gumbel',p,f-g)
    for a in (M('.999999'),M(1),M('1.000001')):
        if a>1:
            show('near collapse gap, scale, ll',*fixed_frechet([M(0),M(0),M(1),M(1)],a))
        else:
            show('collapse exponent',2*a-2)
    for delta in (M('1e-1'),M('1e-4')):
        scan(held_point,[M(0),delta],[M('.5')]*2,'held near constant '+str(delta))

    d=golden('frechet')
    x=list(map(M,d['data']))
    p=[1/M(len(x))]*len(x)
    scan(free_point,x,p,'Fréchet golden')
    scan(held_point,x,p,'Fréchet golden held scale',hold=1)
    show('Fréchet golden held shape=2',*fixed_frechet(x,M(2)))
    centered=[v-min(x) for v in x]
    b0=1/M(d['estimates']['lambda'])
    y0=mp.log((min(x)-M(d['estimates']['mu']))*b0)
    y,b=mp.findroot(lambda y,b:free_point(centered,p,b,y)[1:],(y0,b0))
    v=mp.exp(y)
    shape=1/b
    location=min(x)-v/b
    t=[mp.log1p(b*g/v)/b for g in centered]
    scale=v/b*mean([mp.exp(-f) for f in t],p)**(-b)
    ll=len(x)*free_point(centered,p,b,y)[0]
    show('Fréchet golden public lambda, mu, sigma, ll',shape,location,scale,ll)
    for result, key in zip((shape,location,scale), ('lambda','mu','sigma')):
        assert abs(result-M(d['reference']['estimates'][key])) < M('1e-21')
    assert abs(ll-M(d['reference']['logLik'])) < M('1e-20')
    d=golden('foldnorm')
    x=list(map(M,d['data']))
    p=[1/M(len(x))]*len(x)
    a=mp.findroot(lambda a: folded_joint(x,p,a)[1], (M('1.8'),M('2.1')))
    value,_,mu,t=folded_joint(x,p,a)
    show('foldnorm golden joint mu, sigma, ll',mu,mp.sqrt(t),len(x)*value)
    for result,key in ((mu,'mu'),(mp.sqrt(t),'sigma')):
        assert abs(result-M(d['reference']['estimates'][key])) < M('1e-21')
    assert abs(len(x)*value-M(d['reference']['logLik'])) < M('1e-20')
    show('foldnorm golden sigma held mu=2',mp.sqrt(folded(x,p,M(2))))
    show('foldnorm golden mu held sigma=1',folded_location(x,p,M(1)))
    for delta in (-M('1e-10'),M(0),M('1e-10')):
        show('folded held variance, resulting location',M('.75')+delta,
             folded_location([M(0),M(1)],[M('.25'),M('.75')],M('.75')+delta))
    for mu in (M(0), M('1e-12'), M('0.99'), M(1), M('1.01')):
        t=folded([M(1)],[M(1)],mu)
        show('folded constant hold mu, variance',mu,t)
    p=[M('.99'),M('.01')]
    def tie(a,c):
        x=[M(1),c]
        v,s,_,_=folded_joint(x,p,a)
        return s,v-folded_joint(x,p,0)[0]
    a,c=mp.findroot(tie,(M(3),M(7)))
    show('folded joint local tie candidate a, c',a,c)
    for dy in (-M('1e-8'), M(0), M('1e-8')):
        aa=mp.findroot(lambda aa: tie(aa,c+dy)[0], a)
        show('folded joint near tie c, value gap, interior curvature',
             c+dy,tie(aa,c+dy)[1],mp.diff(lambda b:tie(b,c+dy)[0],aa))
    print('Replay checks passed.', flush=True)
