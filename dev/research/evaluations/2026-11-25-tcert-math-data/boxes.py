"""Numerical implementation of the report's interval formulas and cover.

Weights are exact integer multiplicities before normalization. Interval
endpoints use practical padding in binary64. Independent probes use mpmath.
"""
from collections import Counter
from functools import lru_cache
from math import comb, log, sqrt
from pathlib import Path
import json
import numpy as np
import mpmath as mp
from exact_modes import RADII, COUNTS

EPS = np.finfo(float).eps
ROOT = Path(__file__).resolve().parents[4]
mp.mp.dps = 65


class I:
    def __init__(self, lo, hi=None):
        self.lo = np.asarray(lo, dtype=float)
        self.hi = np.asarray(lo if hi is None else hi, dtype=float)

    @classmethod
    def pad(cls, a, b, allowance=0):
        delta = 64*EPS*np.maximum(abs(a), abs(b)) + allowance + 1e-300
        return cls(a-delta, b+delta)

    @staticmethod
    def cast(x):
        return x if isinstance(x, I) else I(x)

    def __add__(self, b):
        b = I.cast(b)
        return I.pad(self.lo+b.lo, self.hi+b.hi)

    __radd__ = __add__

    def __neg__(self):
        return I(-self.hi, -self.lo)

    def __sub__(self, b):
        return self + (-I.cast(b))

    def __rsub__(self, b):
        return I.cast(b) + (-self)

    def __mul__(self, b):
        b = I.cast(b)
        p = np.array(np.broadcast_arrays(self.lo*b.lo, self.lo*b.hi,
                                         self.hi*b.lo, self.hi*b.hi))
        return I.pad(np.min(p, axis=0), np.max(p, axis=0))

    __rmul__ = __mul__

    def __truediv__(self, b):
        b = I.cast(b)
        if np.any((b.lo <= 0) & (b.hi >= 0)):
            raise ArithmeticError('division through zero')
        return self * I.pad(1/b.hi, 1/b.lo)

    def sq(self):
        lo, hi = self.lo**2, self.hi**2
        a = np.where((self.lo <= 0) & (self.hi >= 0), 0, np.minimum(lo, hi))
        p = I.pad(a, np.maximum(lo, hi))
        return I(np.maximum(0, p.lo), p.hi)

    def exp(self):
        return I.pad(np.exp(self.lo), np.exp(self.hi))

    def mid(self):
        return (self.lo+self.hi)/2

    def mag(self):
        return np.maximum(abs(self.lo), abs(self.hi))

    def sum(self):
        allowance = 64*EPS*np.sum(self.mag())
        return I.pad(np.sum(self.lo), np.sum(self.hi), allowance)


def moments(u, j, power):
    u = np.maximum(u, 0)
    if (j,power) == (1,3):
        return 0.5/(1+u)**2
    small = u <= .125
    s = np.where(small, u, 1)
    terms = [(-1)**k*comb(power+k-1,k)/(j+k+1) for k in range(25)]
    with np.errstate(divide='ignore', invalid='ignore'):
        ratio = u/(1+u)
        num = np.log1p(u) - ratio
        if power == 3:
            num -= .5*ratio**2
        regular = num/u**(j+1)
    return np.where(small, np.polynomial.polynomial.polyval(s,terms), regular)


def weighted_moment(q,h,j,power):
    # q**power J is increasing in q and decreasing in h.
    u, v = h.hi*q.lo,h.lo*q.hi
    a,b = q.lo**power*moments(u,j,power), q.hi**power*moments(v,j,power)
    factor = np.where(((u > .125) & (u <= 1)) | ((v > .125) & (v <= 1)),
                      8e-13, 3e-14)
    return I.pad(a,b,factor*np.maximum(abs(a),abs(b)))


@lru_cache(maxsize=32768)
def norm(h):
    if h == 0:
        return float(-mp.log(2*mp.pi)/2), -.25, 0.
    v = 1/mp.mpf(h)
    p, q = (v+1)/2, v/2
    a = mp.loggamma(p)-mp.loggamma(q)-mp.log(v*mp.pi)/2
    av = (mp.digamma(p)-mp.digamma(q)-1/v)/2
    avv = (mp.polygamma(1,p)-mp.polygamma(1,q))/4+1/(2*v*v)
    return float(a), float(-v*v*av), float(v**4*avv+2*v**3*av)


class Model:
    def __init__(self, x, w, held_m=None, held_scale=None, std=False):
        x, w = np.asarray(x), np.asarray(w, dtype=float)
        self.w = w/w.sum()
        self.center = np.dot(self.w,x)
        self.unit = sqrt(np.dot(self.w,(x-self.center)**2))
        self.x = (x-self.center)/self.unit
        self.held_m = None if held_m is None else (held_m-self.center)/self.unit
        self.held_scale = None if held_scale is None else log(held_scale/self.unit)
        self.std = std
        self.free = [i for i,p in enumerate((held_m,held_scale)) if p is None]
        self.stats = Counter()

    def eta(self,h):
        if self.held_scale is None:
            raise ValueError('free scale')
        if self.std:
            return self.held_scale+.5*np.log1p(-2*h)
        return self.held_scale

    def values(self, m, e, h):
        r = self.x-m
        inv = np.exp(-2*e)
        q = r*r*inv
        d = 1+h*q
        common = (1+h)/d
        val = norm(h)[0]-e-.5*np.dot(self.w,q if h==0 else (1+1/h)*np.log1p(h*q))
        g = np.array([np.dot(self.w,common*r*inv),
                      np.dot(self.w,common*q)-1,
                      norm(h)[1]-.5*np.dot(self.w,q/d-q*q*moments(h*q,1,2))])
        a = np.dot(self.w,-common*(1-h*q)*inv/d)
        b = np.dot(self.w,-2*common*r*inv/d)
        c = np.dot(self.w,-2*common*q/d)
        f = np.dot(self.w,r*(1-q)*inv/d**2)
        j = np.dot(self.w,q*(1-q)/d**2)
        k = norm(h)[2]+np.dot(self.w,q*q*moments(h*q,1,3)-q**3*moments(h*q,2,3))
        H = np.array([[a,b,f], [b,c,j], [f,j,k]])
        if self.std and self.held_scale is not None:
            d1, d2 = -1/(1-2*h), -2/(1-2*h)**2
            H[0,2] += d1*b
            H[2,0] = H[0,2]
            H[2,2] += 2*d1*j+d1*d1*c+d2*g[1]
            g[2] += d1*g[1]
        return val,g,H

    def nuisance_values(self, m, e, h):
        r = self.x-m
        inv = np.exp(-2*e)
        q = r*r*inv
        d = 1+h*q
        common = (1+h)/d
        g = np.array([np.dot(self.w,common*r*inv), np.dot(self.w,common*q)-1])
        H = np.array([[np.dot(self.w,-common*(1-h*q)*inv/d),
                       np.dot(self.w,-2*common*r*inv/d)],
                      [0, np.dot(self.w,-2*common*q/d)]])
        H[1,0] = H[0,1]
        return g,H

    def propose(self,h):
        m = 0. if self.held_m is None else self.held_m
        e = .5*log(np.dot(self.w,(self.x-m)**2)) if self.held_scale is None else self.eta(h)
        if self.held_scale is None:
            for _ in range(150):
                w = self.w*(1+h)/(1+h*(self.x-m)**2*np.exp(-2*e))
                mt = np.dot(w,self.x)/sum(w) if self.held_m is None else m
                et = .5*log(np.dot(w,(self.x-mt)**2))
                diff = max(abs(mt-m), abs(et-e))
                m,e = mt,et
                if diff < 5e-10:
                    break
        idx = self.free
        for _ in range(20):
            g, H = self.nuisance_values(m,e,h)
            step = np.linalg.solve(H[np.ix_(idx,idx)],g[idx])
            if max(abs(step)) < 4e-15:
                break
            p = np.array([m,e])
            p[idx] -= step
            m,e = p
        return m,e

    def interval(self, m, e, h, terms=None):
        if self.held_scale is not None:
            e = I(self.eta(float(h.hi)),self.eta(float(h.lo)))
        r = I(self.x)-m
        inv = (-2*e).exp()
        q = r.sq()*inv
        u = h*q
        d = 1+u
        d2 = d.sq()
        w = I(self.w)
        common = (1+h)/d
        g0 = (w*common*r*inv).sum()
        W = I.pad(q.lo/(1+h.hi*q.lo), q.hi/(1+h.lo*q.hi))
        g1 = (w*(1+h)*W).sum()-1
        a = (w*(-common)*(1-u)*inv/d).sum()
        b = (w*(-2)*common*r*inv/d).sum()
        c = (w*(-2)*common*q/d).sum()
        f = (w*r*(1-q)*inv/d2).sum()
        j = (w*q*(1-q)/d2).sum()
        if self.std and self.held_scale is not None:
            d1 = I(-1)/(1-2*h)
            d2h = I(-2)/(1-2*h).sq()
            f = f+d1*b
        if terms == 'tube':
            return [g0,g1], [[a,b,f], [b,c,j]]
        gp = I.pad(norm(float(h.lo))[1],norm(float(h.hi))[1], 4e-16)
        curvature = norm(float(h.mid()))[2]
        dev = (h.hi-h.lo)/8
        gpp = I.pad(max(0,curvature-dev), min(float(h.hi)/4,curvature+dev), 4e-16)
        g2 = gp-(w*(W-weighted_moment(q,h,1,2))).sum()*.5
        k = gpp+(w*(.5*W.sq()-weighted_moment(q,h,2,3))).sum()
        if self.std and self.held_scale is not None:
            g2 = g2+d1*g1
            k = k+2*d1*j+d1.sq()*c+d2h*g1
        return [g0,g1,g2], [[a,b,f], [b,c,j], [f,j,k]]

    def likelihood_upper(self,m,e,h):
        if self.held_scale is not None:
            e = I(self.eta(float(h.hi)),self.eta(float(h.lo)))
        r = I(self.x)-m
        q = r.sq()*(-2*e).exp()
        # log1p(h*q)/h decreases in h and increases in q.
        a, b = float(h.lo), float(h.hi)
        kernel = q.lo if b==0 else np.log1p(b*q.lo)/b
        return norm(a)[0]-e.lo-(1+a)*np.dot(self.w,kernel)/2+1e-11


def contract(g, H, radius, inverse):
    """Bound displacement under preconditioned gradient steps."""
    extent = []
    for i, row in enumerate(inverse):
        const = sum(c*f for c,f in zip(row,g)).mag()
        defect = [((1 if i==j else 0)-sum(c*col for c,col in zip(row,Hj))).mag()
                  for j,Hj in enumerate(zip(*H))]
        extent.append(const+np.dot(defect,radius))
    return np.array(extent)


def solve_linear(H, u):
    if len(u)==1:
        if H[0][0].hi >= 0:
            raise ArithmeticError('nuisance curvature')
        return [u[0]/H[0][0]]
    a,b = H[0]
    _,c = H[1]
    det = a*c-b.sq()
    if a.hi >= 0 or det.lo <= 0:
        raise ArithmeticError('nuisance curvature')
    return [(c*u[0]-b*u[1])/det, (a*u[1]-b*u[0])/det]


def subset(A, idx):
    return [[A[i][j] for j in idx] for i in idx]


def point_box(p,r):
    return [I(v-d,v+d) for v,d in zip(p,r)]


def tube(model, h, p, radius=None):
    m,e = p
    idx = model.free
    hc = float(h.mid())
    g0,H0 = model.nuisance_values(m,e,hc)
    C = np.linalg.inv(H0[np.ix_(idx,idx)])
    g0 = [I.pad(g0[i],g0[i],2e-13) for i in idx]
    radius = np.full(len(idx),2e-10) if radius is None else np.array(radius)
    # Taylor on the nuisance score at the centre across the shape interval.
    dh = I(float(h.lo)-hc,float(h.hi)-hc)
    _,Hh = model.interval(I(m), I(e), h, 'tube')
    g0 = [g+Hh[i][2]*dh for g,i in zip(g0,idx)]
    radius = np.maximum(radius,1.15*np.array([sum(c*g for c,g in zip(row,g0)).mag()
                                             for row in C]))
    if np.max(radius)>1:
        return None
    for _ in range(3):
        r = np.zeros(2)
        r[idx] = radius
        box = point_box(p,r)
        _, H = model.interval(*box,h,'tube')
        displacement = contract(g0,subset(H,idx),radius,C)
        if np.all(displacement<radius):
            model.stats['tubes'] += 1
            return box
        radius = np.maximum(1.3*radius,1.1*displacement)
        if np.max(radius)>1:
            break
    return None


def profiled(model,Hmax,budget):
    idx = model.free

    @lru_cache(maxsize=None)
    def point(h):
        m,e = model.propose(h)
        box = tube(model,I(h),(m,e))
        if box is None:
            raise ArithmeticError('point tube')
        val,_,_ = model.values(m,e,h)
        maybe, H = model.interval(*box,I(h))
        # Mean-value bound on the gain between the proposal and tube root.
        r = [max(float(box[i].hi-z),float(z-box[i].lo))
             for i,z in enumerate((m,e))]
        error = sum(maybe[i].mag()*r[i] for i in idx)
        if error>=1e-12:
            raise ArithmeticError('conditional point value enclosure')
        return np.array([m,e,h]), val, maybe[2]

    grid = np.unique(np.r_[0,np.geomspace(max(1e-6,Hmax/1024),Hmax,35),Hmax])
    for a in grid:
        point(float(a))
    h0 = max(grid, key=lambda h:point(float(h))[1])
    p, val, _ = point(float(h0))
    p = p.copy()
    winner = 'edge'
    end = h0
    tube_basin = None
    root_widths = None
    if h0 not in (0,Hmax):
        active = idx+[2]
        for _ in range(40):
            val,g,H = model.values(*p)
            step = np.linalg.solve(H[np.ix_(active,active)],g[active])
            if max(abs(step))<2e-14:
                break
            factor = 1.
            while factor>=2**-20:
                trial = p.copy()
                trial[active] -= factor*step
                if 0<trial[2]<Hmax and model.values(*trial)[0]>=val-3e-15:
                    p=trial
                    break
                factor/=2
            else:
                break
        val,g,H = model.values(*p)
        C = np.linalg.inv(H[np.ix_(active,active)])
        rad = np.array([2e-11 if i==0 else (2e-11*abs(p[2]) if i==2 else 2e-11)
                        for i in active])
        for _ in range(16):
            r = np.zeros(3)
            r[active] = rad
            _,Hbox = model.interval(*point_box(p,r))
            gbox,_ = model.interval(*(I(x) for x in p))
            displacement = contract([gbox[i] for i in active],subset(Hbox,active),rad,C)
            if np.all(displacement<rad):
                # Tighten the enclosed root with the very same map bound.
                root_widths = 2*displacement
                break
            rad *= 2
        if root_widths is None:
            raise ArithmeticError('winning root inclusion')
        for fraction in (.05,.02,.01,.005,.002,.001,.0005,.0002,.0001,.00005):
            radius = fraction*min(p[2],Hmax-p[2])
            h = I(p[2]-radius,p[2]+radius)
            q = tube(model,h,p[:2])
            if q is None:
                continue
            _,J = model.interval(*q,h)
            try:
                cross = [J[i][2] for i in idx]
                curvature = J[2][2]-sum(x*y for x,y in zip(cross,solve_linear(subset(J,idx),cross)))
                if curvature.hi < 0 and radius>r[2]:
                    tube_basin = (float(h.lo),float(h.hi))
                    break
            except ArithmeticError:
                pass
        if tube_basin is None:
            raise ArithmeticError(f'winning basin at {p.tolist()}, score {g.tolist()}, '
                                  f'point H {H.tolist()}')
        winner = 'finite'
        end = float(p[2])
    grid = np.unique(np.r_[grid,tube_basin if tube_basin else []])
    pending = [(float(a),float(b)) for a,b in zip(grid,grid[1:])]
    for iteration in range(budget):
        if not pending:
            break
        a,b = pending.pop()
        model.stats['cells'] += 1
        if tube_basin is not None and tube_basin[0]<=a and b<=tube_basin[1]:
            model.stats['basin'] += 1
            continue
        pa, La, ga = point(a)
        pb, Lb, gb = point(b)
        if a>0:
            # Convex-minus-concave chord in nu, inherited from TASK-140.
            top = max(La,Lb)+((b-a)/a)**2/8+2e-11
            if top < val-2e-11:
                model.stats['chord'] += 1
                continue
        c = a+(b-a)/2
        pc,Lc,gc = point(c)
        q = tube(model,I(a,b),pc[:2])
        if q is not None:
            S,J = model.interval(*q,I(a,b))
            g = S[2]
            try:
                cross = [J[i][2] for i in idx]
                curv = J[2][2]-sum(x*y for x,y in zip(cross,solve_linear(subset(J,idx),cross)))
                g = gc+curv*I(a-c,b-c)
            except ArithmeticError:
                pass
            if g.lo>0 or g.hi<0:
                up = Lb if g.lo>0 else La
                # Both ends are conditional roots with validated small tubes.
                if up < val-4e-11 or (winner=='edge' and end==(b if g.lo>0 else a)):
                    model.stats['monotone'] += 1
                    continue
            top = Lc+g.mag()*(b-a)/2+2e-11
            if top<val-2e-11:
                model.stats['profile_value'] += 1
                continue
        if b-a<1e-13:
            raise ArithmeticError('subdivision floor')
        pending.extend([(a,c),(c,b)])
    model.stats['points'] = point.cache_info().currsize
    return {'winner': winner, 'parameters':p.tolist(), 'avg_loglik': val-log(model.unit),
            'root_full_widths': None if root_widths is None else root_widths.tolist(),
            'normal_gap':val-point(0.)[1], 'lower_shape_gap':val-point(float(Hmax))[1],
            'pending':len(pending), 'stats':dict(model.stats)}


def fixed_scale(model,Hmax,seed,budget):
    # Full location-shape enumeration. Negative-definite basin includes one
    # winning root. Shape endpoints require location stationarity only.
    active = [0,2]
    p = np.array([seed[0],0.,seed[1]])
    def at(v):
        v = v.copy()
        v[1] = model.eta(v[2])
        return model.values(*v)
    for _ in range(25):
        val,g,H = at(p)
        step = np.linalg.solve(H[np.ix_(active,active)],g[active])
        p[active] -= step
        if max(abs(step))<3e-14:
            break
    val,g,H = at(p)
    if not 0<p[2]<Hmax:
        raise ArithmeticError('nomination out of domain')
    C = np.linalg.inv(H[np.ix_(active,active)])
    def incl(p,r):
        m,h = point_box(p[active],r)
        g0,_ = model.interval(I(p[0]), I(0), I(p[2]))
        _,J = model.interval(m,I(0),h)
        return contract([g0[a] for a in active],subset(J,active),r,C)
    root_r = np.array([2e-10, 2e-10*p[2]])
    width = 2*incl(p,root_r)
    if np.any(width>=2*root_r):
        raise ArithmeticError('winning root')
    for r0 in (.05,.02,.01,.005,.002,.001,.0005,.0002,.0001):
        r=np.array([r0, min(p[2],Hmax-p[2])*r0])
        m,h=point_box(p[active],r)
        _,J=model.interval(m,I(0),h)
        B=subset(J,active)
        if B[0][0].hi<0 and (B[0][0]*B[1][1]-B[0][1].sq()).lo>0:
            break
    else:
        raise ArithmeticError('no basin')
    limits = (float(min(model.x)),float(max(model.x)))
    grid_m = np.unique(np.r_[limits,m.lo,m.hi])
    grid_h = np.unique(np.r_[0,h.lo,h.hi,Hmax])
    stack = [(a,b,c,d,False) for a,b in zip(grid_m,grid_m[1:])
                              for c,d in zip(grid_h,grid_h[1:])]
    stack += [(a,b,c,c,True) for a,b in zip(grid_m,grid_m[1:]) for c in [0,Hmax]]
    for iteration in range(budget):
        if not stack:
            break
        a,b,c,d,face = stack.pop()
        model.stats['cells'] += 1
        if m.lo<=a and b<=m.hi and h.lo<=c and d<=h.hi:
            model.stats['basin'] += 1
            continue
        M,V = I(a,b),I(c,d)
        if model.likelihood_upper(M,I(0),V)<val-2e-11:
            model.stats['box_value'] += 1
            continue
        G,J = model.interval(M,I(0),V)
        valid_idx=[0] if face else active
        if any(G[i].lo>0 or G[i].hi<0 for i in valid_idx):
            model.stats['score'] += 1
            continue
        cm,ch=float(M.mid()),float(V.mid())
        center,_,_=at(np.array([cm,0,ch]))
        g0,_=model.interval(I(cm),I(0),I(ch))
        offsets=[I(a-cm,b-cm),I(c-ch,d-ch)]
        gt=[g0[i]+sum(J[i][j]*dv for j,dv in zip(active,offsets)) for i in valid_idx]
        if any(z.lo>0 or z.hi<0 for z in gt):
            model.stats['taylor_score'] += 1
            continue
        L=I(center)+sum(g0[i]*dv for i,dv in zip(active,offsets))
        L=L+.5*sum(J[i][j]*(di.sq() if i==j else di*dj)
                   for i,di in zip(active,offsets) for j,dj in zip(active,offsets))
        if L.hi<val-4e-11:
            model.stats['taylor_value'] += 1
            continue
        if max(d-c,b-a)<1e-13:
            raise ArithmeticError('subdivision floor')
        if (b-a)>8*(d-c):
            stack.extend([(a,cm,c,d,face),(cm,b,c,d,face)])
        else:
            stack.extend([(a,b,c,ch,face),(a,b,ch,d,face)])
    return {'parameters':p.tolist(), 'avg_loglik':val-log(model.unit),
            'root_full_widths':width.tolist(), 'pending':len(stack), 'stats':dict(model.stats)}


def load(id):
    return json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions'/id/'golden.json').read_text())


def held_mean_floor(model, witness):
    # t_nu peak over all scales at a nonzero residual <= nu/(2|d|).
    residual = abs(model.x-model.held_m)
    if min(residual)<=0:
        raise ArithmeticError('exact occupied held location')
    return np.exp(witness-1e-11+np.dot(model.w,np.log(residual)))


def held_std_cutoff(model, witness):
    """Exclude nu=2+w, 0<w<=W by an adaptive partition of location."""
    sd = np.exp(model.held_scale)
    count=0
    for exponent in range(1,61):
        W=2.**(-exponent)
        if W>=1:
            continue
        threshold = sd*sqrt(W)
        stack=[(float(min(model.x)),float(max(model.x)))]
        while stack:
            lo,hi=stack.pop()
            count+=1
            distances=np.maximum(0,np.maximum(lo-model.x,model.x-hi))
            good=distances>threshold
            mass=np.sum(model.w[good])
            slope=(3*mass-1)/2
            # gamma ratio peak log coefficient increases in w.
            coefficient=(norm(1/(2+W))[0]+log(2+W)/2-log(sd))
            if slope>0:
                upper=(coefficient+slope*log(W)-
                       3*np.dot(model.w[good],np.log(distances[good]/sd)))
                if upper<witness-1e-10:
                    continue
            if hi-lo<=threshold/8:
                break
            mid=(lo+hi)/2
            stack.extend([(lo,mid),(mid,hi)])
        else:
            return 1/(2+W), count, W
    raise ArithmeticError('held-sd collapse collar')


def workloads():
    for fam, floor in [('lst',1.),('std',2.)]:
        data=load(fam)
        x=data['data']
        est=data['estimates']
        m,s,v = [est[k] for k in ('mu','sigma','df')] if fam=='lst' else [
                    est[k] for k in ('mean','sd','nu')]
        weights=np.ones(len(x))
        std=fam=='std'
        for mask,held_m,held_s in [('all_free',None,None),('held_mean',m,None),
                                     ('held_scale',None,s)]:
            model=Model(x,weights,held_m,held_s,std)
            hmax = 1/floor
            seed=(0.,1/v)
            if mask=='held_mean' and fam=='lst':
                e=model.propose(1/v)[1]
                witness=model.values(model.held_m,e,1/v)[0]
                hmax=1/held_mean_floor(model,witness)
            collar=None
            if mask=='held_scale':
                seed=((m-model.center)/model.unit,1/v)
                if std:
                    witness=model.values(seed[0], model.eta(seed[1]),seed[1])[0]
                    hmax, cells, gap = held_std_cutoff(model,witness)
                    collar={'excluded_w':gap,'cells':cells}
            yield fam+'_'+mask,model,hmax,seed,collar
    for name,rr,ww in [
        ('two_interior_modes',RADII,COUNTS),
        ('normal_local_and_finite_or_lower', [z/100 for z in (1,2,3,4,99,101)],[1]*6),
    ]:
        data=[float(a)*b for a in rr for b in (-1,1)]
        for suffix,h in [('',.5),('_lst',1.)]:
            yield name+suffix,Model(data,np.repeat(ww,2),held_m=0.),h,None,None
    ht=load('ht')
    yield 'ht_golden',Model(ht['data'],np.ones(len(ht['data'])),held_m=0.),None,None,None
    rr=[0.12773,12.5979,3.91782,2.58658,0.138212]
    yield 'ht_two_modes',Model(rr,np.ones(len(rr)),held_m=0.),None,None,None


if __name__ == '__main__':
    import argparse
    parser=argparse.ArgumentParser()
    parser.add_argument('names',nargs='*')
    parser.add_argument('--budget',type=int,default=25600)
    parser.add_argument('--out',type=Path)
    options=parser.parse_args()
    results={}
    for name,model,hmax,seed,collar in workloads():
        if options.names and name not in options.names:
            continue
        print('Running '+name,flush=True)
        if hmax is None:
            hm=1.
            pp=model.propose(hm)
            hmax=1/held_mean_floor(model,model.values(*pp,hm)[0])
        try:
            if model.held_scale is None:
                result=profiled(model,hmax,options.budget)
            else:
                result=fixed_scale(model,hmax,seed,options.budget)
            if name.startswith('ht_'):
                result['avg_loglik']+=log(2.)
            result.update(collar=collar,hmax=hmax,unit=model.unit,center=model.center)
            assert result['pending']==0, (name,result['pending'])
        except (ArithmeticError, ValueError, np.linalg.LinAlgError) as err:
            result={'error':str(err),'stats':dict(model.stats)}
        print(name,json.dumps(result),flush=True)
        results[name]=result
    if options.out:
        options.out.write_text(json.dumps(results,indent=2)+'\n')
    assert all('error' not in r for r in results.values()), 'an enclosure failed'
