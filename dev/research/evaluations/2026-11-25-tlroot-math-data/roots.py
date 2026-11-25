"""Interval certificates for the analytically extended free-scale U profile."""
from collections import Counter
from functools import lru_cache
from fractions import Fraction
from time import perf_counter

import mpmath as mp

from scenarios import cases, fold, point_inverse, terms

iv = mp.iv
mp.mp.dps = 75
iv.dps = 70


def lo(x):
    return mp.mpf(x._mpi_[0])


def hi(x):
    return mp.mpf(x._mpi_[1])


def rat(x):
    x = Fraction(x)
    return iv.mpf(x.numerator)/x.denominator


def sign(x):
    return 1 if lo(x) > 0 else -1 if hi(x) < 0 else 0


def box(a, b):
    return iv.mpf([a, b])


def inverse(t, l):
    """Smaller tail at fixed normalized distance t, including t intervals."""
    d = 1-t
    initial_lo, initial_hi = lo(d/3), min(mp.mpf(".5"), hi(d))
    # Search separately for endpoints with uniformly proved signs.
    relative = max(mp.mpf("1e-55"), hi(l)-lo(l), hi(t)-lo(t))/32
    span = initial_hi-initial_lo
    steps = max(6, min(190, int(mp.ceil(mp.log(span/(relative*initial_lo), 2)))))
    def f(m):
        return iv.exp(l*iv.ln(m)) - iv.expm1(l*iv.log1p(-iv.mpf(m))) - d
    a,b = initial_lo,initial_hi
    for _ in range(steps):
        m = (a+b)/2
        if sign(f(m)) < 0:
            a = m
        else:
            b = m
    left = a
    a,b = left,initial_hi
    for _ in range(steps):
        m = (a+b)/2
        if sign(f(m)) > 0:
            b = m
        else:
            a = m
    return box(left,b)


def parts(x, y, p, q):
    """Partial derivatives F_(shape^i p^j) for F=p^shape-q^shape+t."""
    logp, logq = iv.ln(p), iv.log1p(-p)
    pl, ql = iv.exp(x*logp), iv.exp(x*logq)
    a, b = pl/p, ql/q
    D = a+b
    A = a*logp+b*logq
    B = (x-1)*(a/p-b/q)
    C = a*logp**2+b*logq**2
    E = a/p-b/q+(x-1)*(a/p*logp-b/q*logq)
    G = (x-1)*(x-2)*(a/p**2+b/q**2)
    p1 = -(pl*logp-ql*logq)/(x*D)
    p2 = -(pl*logp**2-ql*logq**2+2*(D+x*A)*p1+x*B*p1**2)/(x*D)
    D1 = A+B*p1
    D2 = C+2*E*p1+G*p1**2+B*p2
    H = iv.ln(2/D)
    S = -D1/D
    S1 = (D1/D)**2-D2/D
    if y < 2:
        return H,S,S1,None,None
    p3 = -(pl*logp**3-ql*logq**3 + 3*(2*A+x*C)*p1
           +3*(B+x*E)*p1**2 + x*G*p1**3
           +3*(D+x*A+x*B*p1)*p2)/(x*D)
    E2 = 2*(a/p*logp-b/q*logq)+(x-1)*(a/p*logp**2-b/q*logq**2)
    G1 = (2*x-3)*(a/p**2+b/q**2)+(x-1)*(x-2)*(a/p**2*logp+b/q**2*logq)
    Gp = (x-1)*(x-2)*(x-3)*(a/p**3-b/q**3)
    D3 = (a*logp**3+b*logq**3 +3*E2*p1 +3*G1*p1**2
          +Gp*p1**3 +3*(E+G*p1)*p2 +B*p3)
    S2 = -D3/D+3*D1*D2/D**2-2*(D1/D)**3
    St = ((E+G*p1)-B*(1/x+2*D1/D))/(x*D**2)
    return H,S,S1,S2,St


class Profile:
    def __init__(self, n, centers, interiors):
        self.n, self.centers, self.interiors = n, centers, interiors
        self.work = 0
        self.points = {}

    @classmethod
    def sample(cls, x):
        x = list(map(Fraction,x))
        mid = (max(x)+min(x))/2
        r = max(x)-mid
        if not r:
            raise ValueError("constant sample")
        counts = Counter(abs(z-mid)/r for z in x)
        return cls(len(x),counts[0],[(rat(t),m) for t,m in counts.items() if 0<t<1])

    def eval(self, a, b, order=2):
        if a == b and a in self.points:
            return self.points[a]
        self.work += 1
        l = box(a,b)
        log2 = iv.ln(2)
        H = -self.n*iv.ln(l)+self.centers*(l-1)*log2
        S = -self.n/l+self.centers*log2
        S1 = self.n/l**2
        S2 = -2*self.n/l**3
        St = iv.mpf(0)
        for t,m in self.interiors:
            p = inverse(t,l)
            v = parts(l,order,p,1-p)
            H += m*v[0]
            S += m*v[1]
            S1 += m*v[2]
            if order == 2:
                S2 += m*v[3]
                St += m*v[4]
        ans = (H,S,S1,S2,St)
        if a == b and order == 2:
            self.points[a] = ans
        return ans

    def local(self,a,b):
        values = self.eval(a,b)
        m = (a+b)/2
        h = box(a,b)-m
        H,S,S1,_,_ = self.eval(m,m)
        curvature = values[3]
        slope = S1+curvature*h
        score = S+S1*h+curvature*h**2/2
        def intersect(x,y):
            return box(max(lo(x),lo(y)),min(hi(x),hi(y)))
        score = intersect(values[1],score)
        slope = intersect(values[2],slope)
        return intersect(values[0],H+score*h),score,slope,curvature,values[4]

    def narrow(self,a,b,sa,sb,goal):
        while b-a > goal:
            # The quarter points handle an exact root at the midpoint.
            for q in (mp.mpf(".5"),mp.mpf(".25"),mp.mpf(".75")):
                m = a+q*(b-a)
                sm = sign(self.eval(m,m)[1])
                if sm:
                    if sm == sa:
                        a = m
                    else:
                        b = m
                    break
            else:
                break
        return a,b

    def isolate(self,a=mp.mpf(1),b=mp.mpf(2),goal=mp.mpf("1e-20")):
        stack = [(a,b)]
        roots, pending = [], []
        while stack:
            a,b = stack.pop()
            _,s,s1,_,_ = self.local(a,b)
            if sign(s):
                continue
            if sign(s1):
                sa,sb = sign(self.eval(a,a)[1]),sign(self.eval(b,b)[1])
                if sa*sb < 0:
                    roots.append(self.narrow(a,b,sa,sb,goal))
                    continue
                if sa == sb and sa:
                    continue
            if b-a <= goal or self.work > 12000:
                pending.append((a,b))
                continue
            # Prefer cuts at which the score has sign, so a root is not
            # stranded at a shared child endpoint.
            for q in (mp.mpf(".5"),mp.mpf(".375"),mp.mpf(".625")):
                m = a+q*(b-a)
                if sign(self.eval(m,m)[1]):
                    break
            stack.extend([(m,b),(a,m)])
        return sorted(roots),pending


def describe(p, roots, pending):
    hs = []
    for a,b in roots:
        H = p.local(a,b)[0]
        hs.append(sign(H))
        print(f"  root {mp.nstr((a+b)/2,23)} width {mp.nstr(b-a,3)}, "
              f"log(P/J) in [{mp.nstr(lo(H),12)}, {mp.nstr(hi(H),12)}]")
    print(f"  roots {len(roots)}, pending {len(pending)}, evaluations {p.work}")
    return hs


def tunable(m,c,t):
    return Profile(2+m+c,c,[(t,m)])


def decimal_bracket(x, digits):
    denom = 10**digits
    f = int(mp.floor(x*denom))
    return (Fraction(f,denom),Fraction(f+1,denom))


def exact_event(name,m,c,l,t,fold_event):
    """Verify an exact implicit input via continuous unique extremum."""
    ta,tb = decimal_bracket(t,28)
    T = box(lo(rat(ta)),hi(rat(tb)))
    base = tunable(m,c,T)
    den = 2**18
    cell = int(mp.floor(l*den))
    a,b = mp.mpf(cell-32)/den,mp.mpf(cell+33)/den
    if fold_event:
        v = base.local(a,b)
        assert sign(v[0]) < 0 and sign(v[3]) < 0 and sign(v[4]) != 0
        print("  fold collar log(P/J) upper",mp.nstr(hi(v[0]),12),
              "score derivative in t sign",sign(v[4]),flush=True)
        derivative = 2
        value = 1
    else:
        assert sign(base.local(a,b)[2]) < 0
        derivative = 1
        value = 0
    assert sign(base.eval(a,a)[derivative]) == 1
    assert sign(base.eval(b,b)[derivative]) == -1
    signs = []
    for endpoint in (ta,tb):
        p = tunable(m,c,rat(endpoint))
        left,right = a,b
        while right-left > mp.mpf("1e-36"):
            mid = (left+right)/2
            s = sign(p.eval(mid,mid)[derivative])
            assert s
            if s > 0:
                left = mid
            else:
                right = mid
        signs.append(sign(p.local(left,right)[value]))
    assert signs[0]*signs[1] == -1
    roots0, pending0 = base.isolate(b=a,goal=mp.mpf("1e-18"))
    roots1, pending1 = base.isolate(a=b,goal=mp.mpf("1e-18"))
    assert not (pending0 or pending1)
    hsigns = describe(base,roots0+roots1,[])
    assert all(x == -1 for x in hsigns)
    print(name, f"t in ({ta}, {tb}), shape collar ({cell-32}/{den}, {cell+33}/{den})",
          "event signs",signs,flush=True)
    return ta,tb


if __name__ == "__main__":
    start = perf_counter()
    print(f"mpmath {mp.__version__}, interval dps {iv.dps}",flush=True)
    for name,sample in cases().items():
        print(name,flush=True)
        p = Profile.sample(sample)
        roots,pending = p.isolate()
        hs = describe(p,roots,pending)
        assert not pending and all(hs)
        assert (1 in hs) == (name in ("witness","separated half tie"))
    m,c = 19,16
    l,t = fold(2,m,c,(mp.mpf("1.8"),mp.mpf(".75")))
    ta,tb = exact_event("exact double root",m,c,l,t,True)
    pair_counts = []
    for t in (ta-Fraction(1,10**18),tb+Fraction(1,10**18)):
        print("near double",t,flush=True)
        p = tunable(m,c,rat(t))
        roots,pending = p.isolate()
        assert not pending and all(s == -1 for s in describe(p,roots,pending))
        pair_counts.append(len(roots))
    assert sorted(pair_counts) == [0,2]

    m,c = 110,35
    n = m+c+2
    def equality(l,t):
        p = point_inverse(t,l)
        H = -n*mp.log(l)+m*mp.log(2/(p**(l-1)+(1-p)**(l-1)))+c*(l-1)*mp.log(2)
        S = -n/l+m*terms(t,l)[0]+c*mp.log(2)
        return H,S
    l,t = mp.findroot(equality,(mp.mpf("1.6"),mp.mpf(".75")))
    ta,tb = exact_event("exact junction equality",m,c,l,t,False)
    for j,r in enumerate((ta-Fraction(1,10**18),tb+Fraction(1,10**18))):
        print("near junction equality",r,flush=True)
        p = tunable(m,c,rat(r))
        roots,pending = p.isolate()
        assert not pending
        assert describe(p,roots,pending) == [-1,2*j-1]
    print(f"All interval checks passed in {perf_counter()-start:.3f}s")
