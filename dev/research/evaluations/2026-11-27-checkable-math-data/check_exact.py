"""Interval replays for the face and exact-sample examples in the report."""
from fractions import Fraction as Q
import importlib.util
from math import exp, log, sqrt
from pathlib import Path

from mpmath import iv, mp
import numpy as np

from pilot import side_log_widths

HERE = Path(__file__).resolve().parent
iv.dps = 80
mp.dps = 95
I = iv.mpf


def load(path):
    spec = importlib.util.spec_from_file_location(path.stem, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def lower_face(h):
    r = 3*h-1
    return (r*iv.log(r)-(r+1)*iv.log(1+r))/2


def normal_face(h):
    return (iv.log(2/iv.pi)-1-iv.log(h))/2


def point_examples():
    for a,b,wanted in ((1,2,"normal"), (4,9,"normal"), (3,8,"two")):
        h = I(a)/b
        gap = lower_face(h)-normal_face(h)
        assert (gap.b < 0 if wanted=="normal" else gap.a > 0)
        print("two-level block:", a, b, wanted, mp.nstr(mp.mpf(gap.mid), 16))

    # Each interior level of the SGED deformation has two copies.
    h = I(1)/6
    def peak(p):
        t = 1/p
        return t*(iv.log(t)-1)-iv.loggamma(1+t)
    max_upper = -1
    for j in range(32, 192):
        lo, hi = I(j)/32, I(j+1)/32
        bound = peak(hi)-iv.log(h+(1-2*h)*iv.exp(-lo*iv.log(2)))/lo
        assert bound.b < 0
        max_upper = max(max_upper, float(bound.b))
    assert iv.log(I(3))-1 > 0  # Centered Laplace witness versus uniform.
    print("sged half bound, head:", max_upper)


def avg(seq):
    return sum(seq)/len(seq)


def interval(x):
    return I(x.numerator)/x.denominator if isinstance(x,Q) else I(x)


def rectangle(a, b):
    return I([interval(a).a, interval(b).b])


def student_normalizer(q):
    return iv.loggamma((q+1)/2)-iv.loggamma(q/2)-iv.log(q*iv.pi)/2


def symmetric_student_examples():
    weights = [I(5)/7, I(2)/7]
    radii = [I(3)/16, I(1)]
    var = sum(w*r*r for w,r in zip(weights,radii))
    kurt = sum(w*r**4 for w,r in zip(weights,radii))/var**2
    assert kurt.b < 3
    normal = -(1+iv.log(2*iv.pi*var))/2

    def student(q, s):
        return student_normalizer(q)-iv.log(s)-(q+1)/2*sum(
            w*iv.log(1+r*r/(q*s*s)) for w,r in zip(weights,radii))

    lo, hi = I("0.25"), I("0.5")
    def scale_score(s):
        return 3*sum(w*r*r/(2*s*s+r*r) for w,r in zip(weights,radii))-1
    assert scale_score(lo).a > 0 and scale_score(hi).b < 0
    lo, hi = mp.mpf(lo.a), mp.mpf(hi.b)
    for _ in range(100):
        mid = (lo+hi)/2
        sign = scale_score(I(mid))
        assert sign.a > 0 or sign.b < 0
        if sign.a > 0:
            lo = mid
        else:
            hi = mid
    two = student(I(2),I([lo,hi]))
    witness = student(I(5)/2,I(9)/25)
    assert witness.a > max(two.b, normal.b)
    print("std four-level witness gaps:", float((witness-two).a), float((witness-normal).a))

    rational = [-4,-3,-2,-1,1,2,3,4,-101,-99,99,101]
    x = [I(i)/100 for i in rational]
    v = avg([a*a for a in x])
    excess = avg([a**4 for a in x])/v**2-3
    improvement = 6*(iv.log(24*v/iv.pi)+1)-2*sum(iv.log(1+3*a*a) for a in x)
    assert excess.b < 0 and improvement.a > I("0.49")
    print("lst clustered sample, witness minus fitted normal:", float(improvement.a))

    # Critical atom example used for the lst collapse detector.
    assert I(3)/20 < 1/iv.pi
    assert iv.sqrt(8/iv.pi)/100 < 1/iv.pi


def entropy(p):
    if p.a == 0 or p.b == 1:
        assert p.a == p.b
        return I(0)
    assert p.a > 0 and p.b < 1
    return -p*iv.log(p)-(1-p)*iv.log(1-p)


def skew_student_examples():
    raw = [-67,-47,-38,-27,-20,-19,-8,-8,-8,-7,-6,-6,-5,0,15,17,22,27,74,85]
    x = [Q(u+67,152) for u in raw]
    n = len(x)
    yi = list(map(interval,x))

    def likelihood(q,c,a,b):
        peak = iv.log(2)+student_normalizer(q)-iv.log(a+b)
        res = [(u-c)/a if u.b < c.a else (u-c)/b for u in yi]
        return peak-(q+1)/2*avg([iv.log(1+r*r/q) for r in res])

    witness = likelihood(I(41)/20,I(77)/200,I(111713)/10**6,I(143348)/10**6)
    threshold = witness.a
    print("sstd rational witness, mean log likelihood:", float(threshold))

    def width_dual(q, left, right):
        # Float widths just propose rational dual coordinates. Mass
        # correction and feasibility are exact before interval evaluation.
        ll, rr = np.array(left,dtype=float)[None,:], np.array(right,dtype=float)[None,:]
        d = [a+b for a,b in zip(left,right)]
        mass = Q(n)/(q+1)
        if sum(u>0 for u in d) <= mass:
            return None
        a,b = side_log_widths(ll,rr,float(q))
        z = ((ll/np.exp(a[:,None])+rr/np.exp(b[:,None]))**2/float(q))[0]
        p = [Q(float(u/(1+u))) for u in z]
        total = sum(p)
        if total >= mass:
            p = [u*mass/total for u in p]
        else:
            room = sum(u>0 for u in d)-total
            p = [u+(int(s>0)-u)*(mass-total)/room for u,s in zip(p,d)]
        assert sum(p) == mass and all(0<=u<=1 for u in p)
        w = sum(u for u,s in zip(p,left) if s>0)/mass
        W = iv.log(2)+student_normalizer(interval(q))-entropy(interval(w))
        terms = [interval(u)*(2*iv.log(interval(s))-iv.log(interval(q)))+entropy(interval(u))
                 for u,s in zip(p,d) if s>0]
        return W-interval(q+1)/(2*n)*sum(terms)

    def distances(lo,hi):
        return [max(lo-u,0) for u in x],[max(u-hi,0) for u in x]

    cuts = sorted(set(x+[Q(i,256) for i in range(257)]))
    normal_upper = -mp.inf
    for lo,hi in zip(cuts,cuts[1:]):
        left,right = distances(lo,hi)
        for_sum = (avg([interval(u)**2 for u in left])**(I(1)/3)
                   +avg([interval(u)**2 for u in right])**(I(1)/3))
        up = (iv.log(2/iv.pi)-1)/2-3*iv.log(for_sum)/2
        normal_upper = max(normal_upper,mp.mpf(up.b))
    assert normal_upper < mp.mpf(threshold)
    print("sstd normal bound margin:",float(mp.mpf(threshold)-normal_upper))

    # Cover the two-piece t2 all the way through the one-sided edges.
    cuts = sorted(set(x+[Q(i,16) for i in range(17)]))
    stack = list(zip(cuts,cuts[1:]))
    checked = 0
    gap = mp.inf
    while stack:
        lo,hi = stack.pop()
        checked += 1
        assert checked <= 12000
        up = width_dual(Q(2),*distances(lo,hi))
        if up is None or up.b >= threshold:
            mid = (lo+hi)/2
            stack.extend(((lo,mid),(mid,hi)))
        else:
            gap = min(gap,mp.mpf(threshold)-mp.mpf(up.b))
    print("sstd t2 cover:",checked,"least accepted margin:",float(gap))

    # Monotone-q half-kernel envelopes. Root signs at dyadic log widths
    # authorize each upper bound, independent of the proposing bisection.
    knots = [mp.mpf(float(k)) for k in 2*np.exp(np.linspace(0,log(256),49))]
    kn = list(zip(knots,[*knots[1:],mp.inf]))
    count = 0
    gap = mp.inf
    while kn:
        a,b = kn.pop()
        count += 1
        assert count <= 1000
        ai = I(a)
        factor = ai if mp.isinf(b) else ai*(1+1/I(b))
        peak = (iv.log(2/iv.pi)/2 if mp.isinf(b) else iv.log(2)+student_normalizer(I(b)))
        candidates = []
        for side in (yi,[1-u for u in yi]):
            zz = [d*d/ai for d in side]
            zz_float = np.array([float(w.mid) for w in zz])
            ff = float(factor.mid)
            lo,hi = -40.,0.
            for _ in range(42):
                mid = (lo+hi)/2
                if ff*(zz_float/(zz_float+exp(2*mid))).mean()>1:
                    lo = mid
                else:
                    hi = mid
            lo,hi = I(lo-1e-9),I(hi+1e-9)
            def sign(t):
                return factor*avg([z/(z+iv.exp(2*t)) for z in zz])-1
            assert sign(lo).a > 0 and sign(hi).b < 0
            candidates.append(peak-lo-factor/2*avg([iv.log(1+z/iv.exp(2*hi)) for z in zz]))
        up = max(v.b for v in candidates)
        if up >= threshold:
            assert not mp.isinf(b)
            mid = mp.mpf(float(sqrt(a*b)))
            kn.extend(((a,mid),(mid,b)))
        else:
            gap = min(gap,mp.mpf(threshold)-mp.mpf(up))
    print("sstd half cover:",count,"least accepted margin:",float(gap))

    # A finite t2 point is above even the best conditional widths allowed
    # on the pilot's finite-shape and join stencil.
    trial_q = [Q(9,4),Q(3),Q(5),Q(10),Q(25),Q(80)]
    joins = sorted(set(x[1:-1]+[(u+v)/2 for u,v in zip(x,x[1:])]))
    upper = max(mp.mpf(width_dual(q,*distances(c,c)).b) for q in trial_q for c in joins)
    centers = np.linspace(.38,.4,81)
    xf = np.array(x,dtype=float)
    left,right = np.maximum(centers[:,None]-xf,0), np.maximum(xf-centers[:,None],0)
    a,b = side_log_widths(left,right,2)
    val = -.5*log(2)-np.logaddexp(a,b)-1.5*np.log1p(
            (left/np.exp(a[:,None])+right/np.exp(b[:,None]))**2/2).mean(axis=1)
    j = val.argmax()
    lower = likelihood(I(2),I(float(centers[j])),I(exp(a[j])),I(exp(b[j])))
    assert lower.a > upper
    print("sstd t2 lower minus all pilot stencil uppers:",float(mp.mpf(lower.a)-upper))


def burr_examples():
    prior = load(HERE.parent/"2026-11-25-burr2-math-data"/"certify.py")
    I = prior.IV
    for q in (prior.M(1)/64, prior.M(1)/32):
        y = prior.pareto_family(I(q))
        pareto = prior.pareto(y)
        _, weibull, _, _ = prior.weibull_bracket(y,I("1e-12"))
        # Use the formal mean-unit tail H(k) <= W + 2*n^2/k for k >= 2*n.
        cutoff = prior.M(512)
        assert (pareto-weibull-2*7**2/I(cutoff)).a > 0
        # Use a left endpoint below the exact 1/6 to cover the seam fully.
        prior.cover(y,prior.M(1)/8,cutoff,pareto.a)
        print("Pareto reference or gap q:", q, "P-W lower:", float((pareto-weibull).a))
    prior.finite_weibull()


if __name__ == "__main__":
    point_examples()
    symmetric_student_examples()
    skew_student_examples()
    burr_examples()
