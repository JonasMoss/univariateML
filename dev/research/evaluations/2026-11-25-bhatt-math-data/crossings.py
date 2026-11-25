"""Numerical branch and grid checks for the three real crossing constructions."""
import math

import mpmath as mp


def q(x):
    return mp.erfc(x / mp.sqrt(2)) / 2


def phi(x):
    return mp.exp(-x*x/2) / mp.sqrt(2*mp.pi)


def logmass(lo, hi):
    if lo >= 0:
        lq = mp.log(q(lo))
        return lq + mp.log(-mp.expm1(mp.log(q(hi)) - lq))
    if hi <= 0:
        return logmass(-hi, -lo)
    return mp.log1p(-(q(-lo) + q(hi)))


def terms(z, k):
    lo, hi = z-k, z+k
    logd = logmass(lo, hi)
    left, right = mp.exp(mp.log(phi(lo))-logd), mp.exp(mp.log(phi(hi))-logd)
    return logd - mp.log(2*k), left+right-1/k, right-left


def held_s(a, d):
    # Mean gain over the normal member at sigma=1, center=0.
    h0, g0, _ = terms(0, a)
    hd, gd, _ = terms(d, a)
    p = mp.mpf(1)/10000
    return (1-p)*h0 + p*hd + mp.log(2*mp.pi)/2 + p*d*d/2, (1-p)*g0+p*gd


def free_s(k, b, t):
    # Mean gain over the uniform limit for x = (+/-1, (+/-t) each 32 times).
    h1, g1, z1 = terms(b, k)
    ht, gt, zt = terms(b*t, k)
    return (h1+32*ht)/33 + mp.log(2*b), (g1+32*gt)/33, 1/b+(z1+32*t*zt)/33


def sparse(k, b, t):
    # Mean gain over the attained normal member for the sparse-spike sample.
    p = mp.mpf(2)**-20
    variance = 1+(1-p)*t*t
    h1, g1, z1 = terms(1024*b, k)
    ht, gt, zt = terms(t*b, k)
    return (p*h1+(1-p)*ht + mp.log(b) + (1+mp.log(2*mp.pi*variance))/2,
            p*g1+(1-p)*gt,
            1/b+1024*p*z1+(1-p)*t*zt)


def inner(fn, k, t, lo, hi):
    # Bisection bracket followed by positive Newton-root recovery.
    assert fn(k, lo, t)[2] > 0 > fn(k, hi, t)[2]
    for _ in range(12):
        mid = (lo+hi)/2
        if fn(k, mid, t)[2] > 0:
            lo = mid
        else:
            hi = mid
    b = mp.findroot(lambda b: fn(k, b, t)[2], (lo, hi),
                    tol=mp.eps*100, verify=False)
    assert b > 0 and abs(fn(k, b, t)[2]) < mp.mpf(10)**(12-mp.mp.dps)
    return b


def show(values):
    print(' '.join(mp.nstr(v, 25) for v in values))


def changes(grid, fn):
    previous = None
    brackets = []
    for x in sorted(set(grid)):
        value = fn(x)
        if previous and value * previous[1] < 0:
            brackets.append((previous[0], x))
        if value != 0:
            previous = (x, value)
    return brackets


def neighbors(x):
    rounded = float(x)
    if mp.mpf(rounded) < x:
        return rounded, math.nextafter(rounded, math.inf)
    return math.nextafter(rounded, -math.inf), rounded


def held_crossing():
    def system(small, large, d):
        hs, gs = held_s(small, d)
        hl, gl = held_s(large, d)
        return gs, gl, hl-hs
    root = mp.findroot(system, (mp.mpf('.07'), 315, 340))
    assert max(map(abs, system(*root))) < mp.eps*1000
    assert 0 < root[0] < 1 < root[1] and 332 < root[2] < 333
    assert all(mp.diff(lambda a: held_s(a, root[2])[1], u) < 0 for u in root[:2])
    show(root)
    show([held_s(root[0], root[2])[0]])
    for d in neighbors(root[2]):
        s = mp.findroot(lambda a: held_s(a, mp.mpf(d))[1], root[0])
        l = mp.findroot(lambda a: held_s(a, mp.mpf(d))[1], root[1])
        print(d.hex(), end=' ')
        show([held_s(l, mp.mpf(d))[0]-held_s(s, mp.mpf(d))[0]])
    return root[2]


def free_crossing(fn, start, upper_t):
    tol = mp.mpf(10)**(16-mp.mp.dps)
    root = mp.findroot(fn, tuple(map(mp.mpf, start)), maxsteps=80, tol=tol)
    k, b, t = root
    assert max(map(abs, fn(*root))) < tol
    assert 0 < k < 132 and b > 0 and mp.mpf('.4') < t < upper_t
    j = mp.calculus.optimization.jacobian(mp.mp, lambda u, v: fn(u, v, t)[1:], (k,b))
    assert j[0,0] < 0 and mp.det(j) > 0
    show([k, b, t, 1/b, k/b])
    for u in neighbors(t):
        k1, b1 = mp.findroot(lambda k, b: fn(k, b, mp.mpf(u))[1:], (k, b))
        print(u.hex(), end=' ')
        show([fn(k1, b1, mp.mpf(u))[0]])
    return t


def main():
    for digits in (70, 105):
        with mp.workdps(digits):
            print('digits', digits, 'held-sigma (small width, large width, outlier)')
            d = held_crossing()
            print('free-sigma (k, b, inner level, sigma, width)')
            t = free_crossing(free_s, ('10', '12', '.7'), 1)
            print('free-sigma against normal (k, b, inner level, sigma, width)')
            ts = free_crossing(sparse, ('3.3', '.67', '3.5'), 10)
    with mp.workdps(55):
        # The peak bound excludes the held-sigma profile above 1000 even at
        # the numerical tie: exp(-mean log L_N)/2 < 1000 there.
        widths = [mp.power(10, -5+mp.mpf(i)/40) for i in range(121)]
        widths += [mp.mpf(i)/100 for i in range(1, 101)]
        widths += [mp.mpf(5*i)/2 for i in range(1, 401)]
        hk = changes(widths, lambda a: held_s(a, d)[1])
        print('held-sigma score brackets, grid points', len(set(widths)))
        for bracket in hk:
            show(bracket)
        assert len(hk) == 3
        widths = [mp.power(10, -4+mp.mpf(i)/25) for i in range(101)]
        widths += [mp.mpf(i)/20 for i in range(1, 161)]
        widths += [mp.mpf(i) for i in range(9, 133)]
        fk = changes(widths, lambda k: free_s(
            k, inner(free_s, k, t, mp.mpf('.5'), 4*(1+k)/t), t)[1])
        print('free-sigma score brackets, grid points', len(set(widths)))
        for bracket in fk:
            show(bracket)
        assert len(fk) == 2
        widths = [mp.power(10, -5+mp.mpf(i)/25) for i in range(126)]
        widths += [mp.mpf(i)/20 for i in range(1, 161)]
        sk = changes(widths, lambda k: sparse(
            k, inner(sparse, k, ts, mp.mpf('.05'), 4*(1+k)/ts), ts)[1])
        print('sparse spike score brackets, grid points', len(set(widths)))
        for bracket in sk:
            show(bracket)
        assert len(sk) == 2


if __name__ == '__main__':
    main()
