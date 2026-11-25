"""Interval checks for an all-free two-winner GEV slice."""
from fractions import Fraction

import mpmath as mp

from replay import high, sample

iv = mp.iv
iv.dps = 40
mp.mp.dps = 60


def scalar(value):
    value = Fraction(value)
    return iv.mpf(value.numerator) / value.denominator


def interval(lo, hi):
    return iv.mpf([scalar(lo).a, scalar(hi).b])


def average(values):
    return sum(values) / len(values)


def weighted(values, weights):
    return sum(x*w for x, w in zip(values, weights)) / sum(weights)


def covariance(x, y, weights):
    return weighted([a*b for a, b in zip(x, y)], weights) - weighted(x, weights)*weighted(y, weights)


def arc(a, z):
    if a == 0:
        return z, -z*z/2
    value = iv.ln(1+a*z)/a
    return value, (z/(1+a*z)-value)/a


def radius_fields(b, z):
    # Both integral expressions are monotone in each argument.
    left, dl = arc(b.b, z.a)
    right, dr = arc(b.a, z.b)
    return iv.mpf([left.a, right.b]), iv.mpf([dr.a, dl.b])


def pivot(data, b, v, curvature=False):
    z = [x/v for x in data]
    t = [1+b*x for x in z]
    if min(y.a for y in t) <= 0:
        return None
    r = [x/y for x, y in zip(z, t)]
    pairs = [radius_fields(b, x) for x in z]
    ell, lb = map(list, zip(*pairs))
    w = [iv.exp(-l) for l in ell]
    val = -iv.ln(v)-iv.ln(average(w))-(1+b)*average(ell)-1
    dy = -1+(1+b)*average(r)-weighted(r, w)
    db = -average(ell)-(1+b)*average(lb)+weighted(lb, w)
    if not curvature:
        return val, dy, db
    ly = [-x for x in r]
    lyy = [x/(y*y) for x, y in zip(z, t)]
    lby = [x*x for x in r]
    lbb = [(-x*x-2*l)/b for x, l in zip(r, lb)]
    yy = -(1+b)*average(lyy)+weighted(lyy, w)-covariance(ly, ly, w)
    by = -average(ly)-(1+b)*average(lby)+weighted(lby, w)-covariance(lb, ly, w)
    bb = -2*average(lb)-(1+b)*average(lbb)+weighted(lbb, w)-covariance(lb, lb, w)
    return val, dy, db, yy, by, bb


def reflected(data, a, e):
    gaps = [e+data[-1]-x for x in data[:-1]]+[e]
    l = [iv.ln(x) for x in gaps]
    w = [iv.exp(a*x) for x in l]
    r = [1/x for x in gaps]
    val = iv.ln(a)+(a-1)*average(l)-iv.ln(average(w))-1
    de = (a-1)*average(r)-a*weighted(r, w)
    da = 1/a+average(l)-weighted(l, w)
    return val, de, da


def center(t, b, negative=False):
    shape = mp.mpf(b.numerator)/b.denominator
    datum = mp.mpf(t.numerator)/t.denominator
    x = sample(datum)
    r = max(x)
    lo, hi = max(mp.mpf(0), -shape*r), 2*r
    for _ in range(40):
        mid = (lo+hi)/2
        z = [d/mid for d in x]
        radii = [a/(1+shape*a) for a in z]
        w = [mp.exp(-mp.log1p(shape*a)/shape if shape else -a) for a in z]
        value = -1+(1+shape)*sum(radii)/4-mp.fdot(radii, w)/sum(w)
        if value > 0:
            lo = mid
        else:
            hi = mid
    return Fraction(str(-hi/shape-r if negative else hi))


def enclosure(t0, t1, b0, b1, negative=False, curvature=False):
    data = sample(interval(t0, t1))
    tm, bm = (t0+t1)/2, (b0+b1)/2
    trial = center(tm, bm, negative)
    b = interval(b0, b1)
    a = -1/b if negative else b
    method = reflected if negative else pivot
    width = max(t1-t0, b1-b0, Fraction(1, 10**10))
    for factor in (Fraction(1, 16), Fraction(1, 4), 1, 4, 16):
        lo, hi = trial-factor*width, trial+factor*width
        if lo <= 0:
            continue
        lower = method(data, a, scalar(lo))
        upper = method(data, a, scalar(hi))
        if lower is not None and upper is not None and lower[1].a > 0 and upper[1].b < 0:
            if curvature:
                return pivot(data, a, interval(lo, hi), True)
            return method(data, a, interval(lo, hi))
    return None


def face(data_parameter):
    return -1-iv.ln(scalar(Fraction(5, 4))-data_parameter)


def verify(t0, t1, b0, b1, criterion, negative=False):
    stack = [(t0, t1, b0, b1)]
    count = 0
    while stack:
        tl, th, bl, bh = stack.pop()
        count += 1
        assert count < 40000, (criterion, tl, th, bl, bh)
        f = enclosure(tl, th, bl, bh, negative, criterion == 'concave')
        if f is not None:
            p = face(interval(tl, th))
            good = {
                'lose': lambda: f[0].b < p.a,
                'decrease': lambda: f[2].b < 0,
                'increase': lambda: f[2].a > 0,
                'concave': lambda: f[3].b < 0 and (f[5]-f[4]**2/f[3]).b < 0,
            }
            # Value exclusion can only replace the sign on outer pieces.
            if good[criterion]() or (criterion != 'concave' and bl != bh and f[0].b < p.a):
                continue
        if th-tl > bh-bl:
            mid = (tl+th)/2
            stack.extend(((tl, mid, bl, bh), (mid, th, bl, bh)))
        else:
            mid = (bl+bh)/2
            stack.extend(((tl, th, bl, mid), (tl, th, mid, bh)))
    print(criterion, str(b0), str(b1), count, flush=True)


def main():
    # Locate the equality numerically first. The interval claims use
    # a rational grid bracket and verify both sides independently.
    root = mp.findroot(
        lambda b, t: (high(sample(t), b)[3],
                      high(sample(t), b)[0] - high(sample(t), -1)[0]),
        (mp.mpf('.35'), mp.mpf('.30')), tol=mp.mpf('1e-45'))
    print('Interior-to-boundary candidate (shape, t)', root, flush=True)
    f = Fraction
    lo, hi = f(29494, 100000), f(29497, 100000)
    print('Rational parameter bracket', lo, hi, flush=True)

    cut = f(-99, 100)
    data = sample(interval(lo, hi))
    cutoff_gap = f(1, 100)
    assert reflected(data, -1/scalar(cut), scalar(cutoff_gap))[1].b < 0
    # For a>=1, bound F_a(1,e) by 1+mean(log w)-weighted(log w,w).
    # For the zero distance use 0 <= e <= cap and the decrease of e log(e).
    cap = scalar(cutoff_gap)
    d = [data[-1]-x for x in data[:-1]]
    log_upper = average([iv.ln(x+cap) for x in d]+[iv.ln(cap)])
    gap = interval(0, cutoff_gap)
    w = [x+gap for x in d]
    numerator = sum(x*iv.ln(x) for x in w)+iv.mpf([(cap*iv.ln(cap)).a, 0])
    assert (1+log_upper-numerator/(sum(w)+gap)).b < 0

    verify(lo, hi, cut, f(-4, 5), 'decrease', True)
    verify(lo, hi, f(-4, 5), f(-3, 5), 'lose', True)
    verify(lo, hi, f(-3, 5), f(0), 'lose')
    b_low, b_high = f(32, 100), f(42, 100)
    verify(lo, hi, f(0), b_low, 'increase')
    verify(lo, hi, b_high, f(1), 'decrease')
    verify(lo, hi, b_low, b_low, 'increase')
    verify(lo, hi, b_high, b_high, 'decrease')
    verify(lo, hi, b_low, b_high, 'concave')

    for t, direction in ((lo, 1), (hi, -1)):
        x = sample(mp.mpf(t.numerator)/t.denominator)
        b = mp.findroot(lambda b: high(x, b)[3], (root[0]-.01, root[0]+.01))
        mid = f(str(b))
        step = f(1, 10**9)
        cell = enclosure(t, t, mid-step, mid+step)
        assert cell is not None
        assert enclosure(t, t, mid-step, mid-step)[2].a > 0
        assert enclosure(t, t, mid+step, mid+step)[2].b < 0
        difference = cell[0] - face(scalar(t))
        assert (direction*difference).a > 0
        print('Endpoint difference', t, difference, flush=True)
    print('All interval checks passed', flush=True)


if __name__ == '__main__':
    main()
