"""Enclose all stationary roots and compare all feasible peak values."""
from collections import Counter
from fractions import Fraction as Q
from functools import lru_cache
from itertools import combinations_with_replacement
from math import ceil, factorial
import mpmath as mp

mp.iv.dps = 60
mp.mp.dps = 60
SCALE = 1 << 224
iv = mp.iv


def rational(x):
    x = Q(x)
    return iv.mpf(x.numerator) / x.denominator


def endpoints(a, b):
    return iv.mpf([a.a, b.b])


@lru_cache(maxsize=16000)
def ratio_bounds(u, k_max):
    """Enclose A_k for 0 <= k <= k_max using directed integer division."""
    w = 4*u
    wn, wd = w.numerator, w.denominator
    # Depth affects cost and width only. The terminal enclosure is valid at any depth.
    last = k_max + max(32, ceil(5*float(u)**0.5) + 8)
    lo, hi = 0, ceil(w*SCALE/(2*(last+1)))
    out = [None]*(k_max+1)
    for k in range(last-1, 0, -1):
        c = 2*(k+1)*SCALE
        lo, hi = ((wn*SCALE*SCALE)//(wd*(c+hi)),
                  -((-wn*SCALE*SCALE)//(wd*(c+lo))))
        if k <= k_max+1:
            c = 2*k*SCALE
            out[k-1] = (Q(2*SCALE, c+hi), Q(2*SCALE, c+lo))
    return out


class Profile:
    def __init__(self, d, weights):
        self.d = Q(d)
        self.weights = weights
        self.kmax = max(weights)
        self.m = (sum(k*w for k,w in weights.items())-d)/2
        self.p = self.m+d
        self.terms = [(k,rational(w)) for k,w in weights.items()]
        self.di, self.mi, self.pi = map(rational, (self.d,self.m,self.p))
        assert 0 <= self.m and sum(weights.values()) == 1
        self.far = max(32, 2*self.kmax, 2*sum(k*k*w for k,w in weights.items())+2)

    def a_bounds(self, a, b):
        low = ratio_bounds(b*(b+self.d), self.kmax+1)
        high = ratio_bounds(a*(a+self.d), self.kmax+1)
        return {k: endpoints(rational(low[k][0]), rational(high[k][1]))
                for k in set(self.weights) | {k+1 for k in self.weights}}

    def score(self, a, b, derivative=False):
        t = endpoints(rational(a), rational(b))
        h = 2*t+self.di
        ak = self.a_bounds(a,b)
        if derivative:
            result = 2*sum(w*ak[k] for k,w in self.terms)
            result += h*h*sum(w*ak[k]*(ak[k+1]-ak[k]) for k,w in self.terms)
            if self.p:
                result -= self.pi/(t+self.di)**2
            if self.m:
                result -= self.mi/t**2
            return result
        result = -2+h*sum(w*ak[k] for k,w in self.terms)
        if self.p:
            # For a mixed sample, a half-infinite score bound at zero suffices.
            result += self.pi/(t+self.di)
        if self.m:
            result += self.mi/t
        return result

    def value(self, a, b):
        t = endpoints(rational(a),rational(b))
        u = t*(t+self.di)
        ans = -2*t-self.di
        if self.p:
            ans += self.pi*iv.ln(t+self.di)
        if self.m:
            ans += self.mi*iv.ln(t)
        for k,w in self.terms:
            term = series = iv.mpf(1)
            for j in range(1,10000):
                term *= u/(j*(j+k))
                series += term
                rho = u/((j+1)*(j+k+1))
                if rho.b < 1 and (term*rho/(1-rho)).b < series.a*iv.mpf("1e-56"):
                    remainder = term*rho/(1-rho)
                    series += endpoints(iv.mpf(0),remainder.b)
                    break
            else:
                raise ArithmeticError("series budget")
            ans += w*(iv.ln(series)-iv.ln(factorial(k)))
        return ans


def sign(x):
    return 1 if x > 0 else -1 if x < 0 else 0


def isolate(profiles, windows=()):
    """For every profile in the convex hull, exclude roots outside windows.

    With no windows, isolate each root for the single supplied profile.
    Window endpoints and the complement cover the entire finite interval.
    """
    p = profiles[0]
    assert all(q.d == p.d and q.m > 0 for q in profiles) or len(profiles)==1
    far = Q(max(q.far for q in profiles),2)
    a = min(Q(1,32), min((q.m/4 for q in profiles if q.m), default=Q(1,32)))
    if not p.m:
        g0 = p.d*sum(w/(k+1) for k,w in p.weights.items())-1
        for _ in range(100):
            if sign(p.score(0,a)) or (g0 == 0 and sign(p.score(0,a,True))):
                break
            a /= 2
        else:
            raise ArithmeticError("face unresolved")
    else:
        assert all(q.m/a >= 4 for q in profiles)  # g >= -2 + m/t
    grid = [a]
    while grid[-1] < far:
        grid.append(min(far,2*grid[-1]))

    @lru_cache(maxsize=None)
    def point(x):
        signs = [sign(q.score(x,x)) for q in profiles]
        return signs[0] if all(s==signs[0] for s in signs) else 0

    def cell_sign(a,b,derivative=False):
        signs = [sign(q.score(a,b,derivative)) for q in profiles]
        return signs[0] if all(s==signs[0] for s in signs) else 0

    roots = []
    if windows:
        assert a < windows[0][0] and windows[-1][1] < far
        for left,right,curvature in windows:
            assert point(left)==-curvature and point(right)==curvature
            cells = [(left,right,0)]
            while cells:
                l,r,depth = cells.pop()
                if cell_sign(l,r,True)==curvature:
                    continue
                assert depth < 24, (l,r,"window curvature")
                mid = (l+r)/2
                cells.extend(((l,mid,depth+1),(mid,r,depth+1)))
            if curvature < 0:
                roots.append((left,right))
        active = [a] + [x for left,right,_ in windows for x in (left,right)] + [far]
        pairs = []
        for left,right in zip(active[::2],active[1::2]):
            nodes = [left] + [x for x in grid if left<x<right] + [right]
            pairs.extend(zip(nodes,nodes[1:]))
    else:
        pairs = zip(grid,grid[1:])

    cells = [(a,b,0) for a,b in pairs]
    while cells:
        a,b,depth = cells.pop()
        if cell_sign(a,b):
            continue
        sa,sb = point(a),point(b)
        if sa and sb and ((sa==sb and cell_sign(a,b,True)) or
                          (sa!=sb and cell_sign(a,b,True)==sb)):
            if sa!=sb:
                assert not windows, (a,b,"unexpected root")
                if sa>0:
                    roots.append((a,b))
            continue
        assert depth < 30, (p.d,p.weights,a,b,"unresolved root cover")
        mid = (a+b)/2
        cells.extend(((a,mid,depth+1),(mid,b,depth+1)))
    return sorted(roots)


def compare(profile, brackets):
    if not profile.m:
        brackets = [(Q(0),Q(0))] + brackets
    if len(brackets)==1:
        return 0, None
    assert brackets
    values = []
    for a,b in brackets:
        if a != b:
            for _ in range(108):
                c = (a+b)/2
                s = sign(profile.score(c,c))
                assert s, ("root refinement unresolved",c)
                if s>0:
                    a = c
                else:
                    b = c
        values.append(profile.value(a,b))
    wins = [i for i,v in enumerate(values)
            if all(i==j or v>w for j,w in enumerate(values))]
    assert len(wins)==1, ("unresolved peak comparison",profile.weights,values)
    return wins[0], values


def small_samples():
    keys = set()
    samples = symmetric = zero_mean = constant = 0
    maxima_counts = Counter()
    for order, n_max in ((4,6),(6,4)):
        for n in range(2,n_max+1):
            for x in combinations_with_replacement(range(-order,order+1),n):
                if order==6 and max(map(abs,x))<=4:
                    continue
                samples += 1
                symmetric += x==tuple(-v for v in reversed(x))
                zero_mean += sum(x)==0
                if x[0]==x[-1]:
                    constant += 1
                    continue
                d = Q(abs(sum(x)),n)
                hist = Counter(map(abs,x))
                key = d,tuple(sorted((k,Q(v,n)) for k,v in hist.items()))
                if key in keys:
                    continue
                keys.add(key)
                profile = Profile(d,dict(key[1]))
                roots = isolate([profile])
                winner,values = compare(profile,roots)
                # A feasible face with negative inward slope is a local maximum.
                g0 = d*sum(w/(k+1) for k,w in profile.weights.items())-1
                face = not profile.m and (g0<0 or
                    (g0==0 and sign(profile.score(0,0,True))<0))
                count = len(roots)+int(face)
                maxima_counts[count] += 1
                if count>1:
                    assert len(values)==2
                    print("multiple local maxima, sample",x,"winner",winner,
                          "gap",values[1]-values[0],flush=True)
    print("small samples",samples,"symmetric",symmetric,"zero mean",zero_mean,
          "constant",constant,"distinct nonconstant profiles",len(keys),
          "local-maximum counts",dict(sorted(maxima_counts.items())),flush=True)


def rational_neighbors(value, max_denominator):
    """The two bounding continued-fraction approximants at a denominator cap."""
    p0, q0, p1, q1 = 0, 1, 1, 0
    n, d = value.numerator, value.denominator
    assert d > max_denominator
    while True:
        a, r = divmod(n,d)
        q2 = q0+a*q1
        if q2 > max_denominator:
            break
        p0, q0, p1, q1 = p1, q1, p0+a*p1, q2
        n, d = d, r
    j = (max_denominator-q0)//q1
    lower, upper = sorted((Q(p0+j*p1,q0+j*q1),Q(p1,q1)))
    assert lower < value < upper
    return lower, upper


def coexistence(epsilon, d):
    k = 40

    def profile(w):
        weights = {0:1-w-epsilon,k:w}
        if epsilon:
            weights[1] = epsilon
        return Profile(d,weights)

    e = mp.mpf(epsilon.numerator)/epsilon.denominator
    mean = mp.mpf(d.numerator)/d.denominator

    def score(z,w):
        def h(order):
            return order+z*mp.besseli(order+1,z)/mp.besseli(order,z)
        return (1-w-e)*h(0)+e*h(1)+w*h(k)-mp.sqrt(z*z+mean*mean)

    def value(z,w):
        h = mp.sqrt(z*z+mean*mean)
        return ((1-w-e)*mp.log(mp.besseli(0,z))+e*mp.log(mp.besseli(1,z))
                +w*mp.log(mp.besseli(k,z))-h+mean*mp.log((h+mean)/z))

    z1,z2,w = mp.findroot(lambda a,b,c: (score(a,c),score(b,c),value(b,c)-value(a,c)),
                           (mp.mpf("1.34"),mp.mpf("6.8"),mp.mpf(".0152")))
    print("point nomination epsilon, d, z1, z2, w",epsilon,d,
          *(mp.nstr(x,58) for x in (z1,z2,w)),flush=True)
    minimum = mp.findroot(lambda x: score(x,w),(2,5),solver="bisect",maxsteps=240)
    windows = []
    for z,curvature in zip((z1,minimum,z2),(-1,1,-1)):
        center = Q(int(1000*(mp.sqrt(z*z+mean*mean)-mean)/2),1000)
        windows.append((center-Q(1,50),center+Q(1,50),curvature))
    center = int(w*10**7)
    extremes = [profile(Q(center+j,10**7)) for j in (-1,2)]
    roots = isolate(extremes,windows)
    print("uniform root windows in t",windows,flush=True)
    for i,p in enumerate(extremes):
        winner,vals = compare(p,roots)
        print("coexistence endpoint",epsilon,d,p.weights[k],
              "winner",winner,"gap",vals[1]-vals[0],flush=True)
        assert winner==i
    return profile,w,windows


def close_rational(profile, w, windows):
    for cap in (10**6,10**12):
        for i,half_weight in enumerate(rational_neighbors(Q(mp.nstr(w/2,59)),cap)):
            p = profile(2*half_weight)
            roots = isolate([p],windows)
            assert len(roots)==2
            winner,vals = compare(p,roots)
            assert winner==i
            print("symmetric integer histogram",half_weight.denominator-2*half_weight.numerator,
                  half_weight.numerator,half_weight.numerator,
                  "winner",winner,"gap",vals[1]-vals[0],flush=True)


def face_coexistence():
    def components(t):
        z = 2*mp.sqrt(t*(t+1))
        h = 2*t+1
        angle = mp.log((h+1)/z)

        def score(k):
            return k+z*mp.besseli(k+1,z)/mp.besseli(k,z)-h

        def gain(k):
            return mp.log(mp.besseli(k,z))+angle-2*t+mp.log(mp.factorial(k))

        s0, s1, s20 = (score(k) for k in (0,1,20))
        v0, v1, v20 = (gain(k) for k in (0,1,20))
        return s1,(19*s0+s20)/20-s1,v1,(19*v0+v20)/20-v1

    def equations(t,w):
        base,change,gain,delta = components(t)
        return base+w*change,gain+w*delta

    t,w = mp.findroot(equations,(mp.mpf("5"),mp.mpf(".95")))
    print("face coexistence point nomination t, weight",mp.nstr(t,58),mp.nstr(w,58),
          flush=True)
    for i,weight in enumerate(rational_neighbors(Q(mp.nstr(w,59)),10**6)):
        n,d = weight.numerator,weight.denominator
        profile = Profile(Q(1),{0:19*weight/20,1:1-weight,20:weight/20})
        roots = isolate([profile])
        assert len(roots)==1
        winner,vals = compare(profile,roots)
        assert winner==i
        print("one-sided integer histogram",19*n,20*(d-n),n,
              "winner",winner,"gap",vals[1]-vals[0],flush=True)


if __name__ == "__main__":
    import sys
    if sys.argv[1:] == ["small"]:
        small_samples()
    elif sys.argv[1:] == ["coexistence"]:
        close_rational(*coexistence(Q(0),Q(0)))
        coexistence(Q(1,10000),Q(1,100000))
        face_coexistence()
    else:
        raise SystemExit("expected small or coexistence")
