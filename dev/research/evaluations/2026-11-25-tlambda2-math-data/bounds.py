"""Free-shape modulus checks and a correlated log-likelihood box probe."""
from itertools import product

import mpmath as mp

iv = mp.iv
mp.mp.dps = 75
iv.dps = 60


def endpoints(x):
    return tuple(mp.mpf(t) for t in x._mpi_)


def box(lo, hi):
    return iv.mpf([lo, hi])


def q(u, shape, ctx=mp):
    if shape == 0:
        return ctx.ln(u) - ctx.log1p(-u)
    return (ctx.expm1(shape * ctx.ln(u))
            - ctx.expm1(shape * ctx.log1p(-u))) / shape


def inverse(z, shape):
    if z == 0:
        return mp.mpf(".5")
    if z < 0:
        return 1 - inverse(-z, shape)
    if shape > 0 and z >= 1/shape:
        return mp.mpf(1)
    if shape == 0:
        return 1 / (1 + mp.exp(-z))
    # Bisect the smaller tail. Stop when arithmetic cannot subdivide,
    # or at more than the point precision in bits.
    low, high = mp.mpf(0), mp.mpf(".5")
    for _ in range(mp.mp.prec + 16):
        mid = (low + high) / 2
        if not low < mid < high:
            break
        if -q(mid, shape) > z:
            low = mid
        else:
            high = mid
    return 1 - (low + high) / 2


def inverse_at_center(z, shape):
    # The point solve proposes an interval. Endpoint signs validate it.
    u = inverse(z, shape)
    delta = mp.mpf("1e-55")
    for _ in range(4):
        a, b = u - delta, u + delta
        if 0 < a < b < 1:
            zs = iv.mpf(z)
            if endpoints(q(iv.mpf(a), iv.mpf(shape), iv) - zs)[1] < 0:
                if endpoints(q(iv.mpf(b), iv.mpf(shape), iv) - zs)[0] > 0:
                    return a, b
        delta *= 8
    raise ArithmeticError("inverse sign check failed")


def g(u, shape):
    p = min(u, 1 - u)
    if not p:
        return mp.mpf(0)
    return 1 / (p**(shape - 1) + (1 - p)**(shape - 1))


def probe_modulus():
    C = (1 + mp.log(2)) / 2
    max_u = max_g = mp.mpf(0)
    count = 0
    for delta in map(mp.mpf, ("1", ".5", ".125", ".03125")):
        shapes = (-mp.mpf(1), -mp.mpf(".25"), mp.mpf(0), 1-delta)
        for shape, z_text in product(shapes, ("0", ".5", ".99", "1", "1.01", "3", "250")):
            z = mp.mpf(z_text)
            u = inverse(z, shape)
            for step in map(mp.mpf, ("1e-2", "1e-4", "1e-8")):
                target = max(-1, shape-step) if shape > -1 else shape+step
                v = inverse(z+step, target)
                w = min(mp.mpf(".5"), step/2+C*abs(target-shape))
                du = abs(u-v) / w  # u,v are both in the upper half
                dg = abs(g(u,shape)-g(v,target)) / (
                    max(w**delta, 2*w) + abs(target-shape)/(mp.e*delta))
                max_u, max_g = max(max_u, du), max(max_g, dg)
                assert du <= 1 and dg <= 1
                count += 1
    print(f"{count} modulus comparisons, probability/bound {mp.nstr(max_u,10)}, "
          f"density/bound {mp.nstr(max_g,10)}")

    # Given (-1,1), the support contacts at scale=shape give zero
    # likelihood below one, and positive likelihood at one.
    for j in (4, 12, 30):
        shape = 1 - mp.mpf(2)**(-j)
        assert g(inverse(1/shape, shape), shape) == 0
    print("Support-contact discontinuity control passed.")


def terms(u, shape, ctx):
    v = 1-u
    a, b = u**(shape-1), v**(shape-1)
    D = a+b
    Du = (shape-1)*(a/u-b/v)
    Dl = a*ctx.ln(u)+b*ctx.ln(v)
    Ql = (shape*(u*a*ctx.ln(u)-v*b*ctx.ln(v))-(u*a-v*b))/shape**2
    hz = -Du/D**2
    return -ctx.ln(D), hz, -Dl/D-hz*Ql


def point(x, A, B, shape):
    terms_at_x = [terms(inverse(t*A+B, shape),shape,mp) for t in x]
    value = len(x)*mp.log(A)+sum(r[0] for r in terms_at_x)
    score = (len(x)/A+sum(t*r[1] for t,r in zip(x,terms_at_x)),
             sum(r[1] for r in terms_at_x), sum(r[2] for r in terms_at_x))
    return value, score


def box_bound(x, center, h, roots):
    A, B, shape = center
    ai, li = box(A-h,A+h), box(shape-h,shape+h)
    ai0, li0 = iv.mpf(A), iv.mpf(shape)
    val, grad = len(x)*iv.ln(ai0), [len(x)/ai,iv.mpf(0),iv.mpf(0)]
    corner = len(x)*iv.ln(ai)
    # 0.847 is strictly larger than (1+log(2))/2.
    for t, (a,b) in zip(x,roots):
        rad = (abs(t)+1)*h/2 + mp.mpf(".847")*h
        _, hz, hl = terms(box(a-rad,b+rad), li, iv)
        val += terms(box(a,b), li0, iv)[0]
        corner += terms(box(a-rad,b+rad), li, iv)[0]
        grad[0] += hz*t
        grad[1] += hz
        grad[2] += hl
    correlated = val + sum(gg*box(-h,h) for gg in grad)
    return endpoints(val), endpoints(correlated)[1], endpoints(corner)[1]


def probe_box():
    # A symmetric, exactly specified lattice rounding of the earlier
    # quantile probe. This is a separate sample from its binary64 probe.
    n, lattice = 31, 2**44
    shape = mp.mpf(7)/50
    pos = [mp.nint(q(mp.mpf(2*i+1)/(2*n),shape)*lattice)/lattice
           for i in range(n//2+1,n)]
    x = [-p for p in reversed(pos)]+[mp.mpf(0)]+pos
    print("Exact probe: numerator over 2^44, positive half:")
    print(" ".join(str(int(z*lattice)) for z in pos))
    def score(A, l):
        a, _, b = point(x,A,0,l)[1]
        return a,b
    # Numerically propose the local stationary point. The box bounds
    # use a rounded rational anchor and have independent inverse checks.
    guess = mp.findroot(score,(mp.mpf(".92"),mp.mpf(".22")),tol=mp.mpf("1e-62"))
    scale = mp.mpf(10)**30
    A, l = (mp.nint(v*scale)/scale for v in guess)
    center = (A, mp.mpf(0), l)
    roots = [inverse_at_center(t*A,l) for t in x]
    print("Anchor A, B, lambda:",*(mp.nstr(v,32) for v in center))
    print("h          correlated excess   independent excess   corner max gain")
    last = None
    target = mp.mpf("3.1e-6")
    crossed = False
    for h in map(mp.mpf, ("1e-3", "1e-4", "1e-5", "1e-6", "1e-7")):
        (va, vb), upper, sep = box_bound(x,center,h,roots)
        corner_max = max(point(x,*(v+s*h for v,s in zip(center,signs)))[0]
                         for signs in product((-1,1),repeat=3))
        assert corner_max <= upper
        excess, independent = upper-va, sep-va
        assert 0 < excess < independent
        if last is not None:
            assert last/excess > 80
        crossed |= excess < target < independent
        last = excess
        print(*(mp.nstr(v,11) for v in (h,excess,independent,corner_max-vb)))
    assert crossed


if __name__ == "__main__":
    print(f"mpmath {mp.__version__}, point digits {mp.mp.dps}, interval digits {iv.dps}",
          flush=True)
    probe_modulus()
    probe_box()
    print("All free-shape bound checks passed.")
