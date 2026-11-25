"""Numerical replay of the exact-arithmetic local score-box certificate."""
from itertools import product

import mpmath as mp

from check import exact_eval, normal_interval, replay, root


def bounds(v):
    return mp.mpf(v.a), mp.mpf(v.b)


def iv(lo, hi=None):
    return mp.iv.mpf([lo, lo if hi is None else hi])


def magnitude(v):
    return max(map(abs, bounds(v)))


def padded(lo, hi):
    pad = mp.mpf("1e-80") * max(abs(lo), abs(hi))
    return iv(lo-pad, hi+pad)


def density(v):
    lo, hi = bounds(v)
    near = 0 if lo <= 0 <= hi else min(abs(lo), abs(hi))
    far = max(abs(lo), abs(hi))
    return padded(mp.exp(-far**2/2)/mp.sqrt(2*mp.pi),
                  mp.exp(-near**2/2)/mp.sqrt(2*mp.pi))


def score_hessian(hist, center, vals, mean, sd):
    npar = len(vals)
    gradient = [iv(0) for _ in vals]
    hessian = [[iv(0) for _ in vals] for _ in vals]
    if mean is not None:
        B, = vals
    elif sd is not None:
        A, = vals
        B = 1/iv(sd)
    else:
        A, B = vals
    for x, count in hist.items():
        if mean is not None:
            l, r = iv(x)-iv(mean), iv(x+1)-iv(mean)
            a, b = l*B, r*B
            va, vb = [l], [r]
        else:
            l, r = iv(x)-iv(center), iv(x+1)-iv(center)
            a, b = l*B-A, r*B-A
            va, vb = ([-1], [-1]) if sd is not None else ([-1, l], [-1, r])
        al, ah = bounds(a)
        bl, bh = bounds(b)
        assert ah < bl
        p = padded(normal_interval(ah, bl, mp), normal_interval(al, bh, mp))
        fa, fb = density(a)/p, density(b)/p
        score = [fb*vb[i]-fa*va[i] for i in range(npar)]
        for i, j in product(range(npar), repeat=2):
            hessian[i][j] += count*(score[i]*score[j]+b*fb*vb[i]*vb[j]-a*fa*va[i]*va[j])
        for i in range(npar):
            gradient[i] -= count*score[i]
    return gradient, hessian


def certificate(hist, coefficients, center, mean=None, sd=None):
    """Use boxes with a common scaled radius in working coordinates."""
    mean, sd = (mp.mpf(v) if v is not None else None for v in (mean, sd))
    mu, s = map(mp.mpf, coefficients)
    point = [1/s] if mean is not None else [(mu-center)/s]
    if mean is None and sd is None:
        point.append(1/s)
    radii = [mp.mpf("1e-12") * (abs(t) if mean is not None or i == 1 else 1)
             for i, t in enumerate(point)]
    coords = range(len(point))
    p = [iv(t) for t in point]
    box = [t + iv(-r, r) for t, r in zip(p, radii)]
    g, at_point = score_hessian(hist, center, p, mean, sd)
    _, H = score_hessian(hist, center, box, mean, sd)
    midpoint = mp.matrix([[sum(bounds(entry))/2 for entry in row] for row in at_point])
    M = [[iv(v) for v in row] for row in (midpoint**-1).tolist()]
    ratios = []
    for i in coords:
        step = magnitude(sum(M[i][k]*g[k] for k in coords))
        radius = step + sum(magnitude(int(i == j)-sum(M[i][k]*H[k][j] for k in coords))
                            * radii[j] for j in coords)
        ratios.append(radius/radii[i])
    if max(ratios) >= 1:
        return max(ratios), None
    if mean is not None:
        ranges = (iv(mean), 1/box[0])
    elif sd is not None:
        ranges = (iv(center)+box[0]*iv(sd), iv(sd))
    else:
        ranges = (iv(center)+box[0]/box[1], 1/box[1])
    # Compare against the public pair, including a separate binary64 rounding.
    ml, mh = bounds(ranges[0])
    sl, _ = bounds(ranges[1])
    min_abs_mu = 0 if ml <= 0 <= mh else min(abs(ml), abs(mh))
    units = (max(min_abs_mu, sl), sl)
    error_upper = max(magnitude(v-iv(float(z)))/unit for z, v, unit in
                      zip(coefficients, ranges, units))
    return max(ratios), error_upper


def main():
    mp.mp.dps = 100
    mp.iv.dps = 110
    success = 0
    for gap, count, offset in product((2, 3, 8), (1, 2, 9), (0, 2**51)):
        hist = {offset: count, offset+gap: 1}
        center = mp.mpf(offset) + mp.mpf(gap+1)/2
        for holds in ({}, {"mean": center}, {"sd": mp.mpf(gap)/2}):
            start, status, _ = replay(hist, num=mp, **holds)
            assert status == "decrement", (gap, count, offset, holds)
            star = root(hist, holds, start)
            ratio, error = certificate(hist, star, center, **holds)
            assert ratio < 1 and error < mp.mpf("1e-10")
            success += 1
    for exponent in (52, 60, 100):
        h = -mp.mpf(2)**(-exponent)
        star = h, mp.sqrt((1-2*h)/(2*mp.log1p(-1/h)))
        ratio, error = certificate({0: 1}, star, mp.mpf("0.5"), mean=h)
        assert ratio < 1 and error < mp.mpf("1e-10")
        wrong_ratio, _ = certificate({0: 1}, (h, mp.mpf("0.103")), mp.mpf("0.5"), mean=h)
        assert wrong_ratio > 1
        success += 1
        print("held seam", exponent, "correct-box ratio", mp.nstr(ratio, 6),
              "wrong-box ratio", mp.nstr(wrong_ratio, 6))
    print(success, "score boxes passed the exact-arithmetic inclusion test with numerical bounds")
    with mp.workdps(280):
        for e in (3, 4, 5):
            s = mp.mpf(2)**(-e)
            h = 1-s*mp.sqrt(2)*mp.erfinv(mp.mpf("0.4"))
            ll = exact_eval({0: 7, 1: 3}, h, s)[0]
            face = 7*mp.log(mp.mpf("0.7"))+3*mp.log(mp.mpf("0.3"))
            assert ll < face
            print("adjacent face", e, "strict loss", mp.nstr(face-ll, 10))
        for hist, mu, face in (({0: 1}, mp.mpf("0.5"), 0),
                               ({0: 1}, 0, -mp.log(2)),
                               ({0: 7, 1: 3}, 1, -10*mp.log(2))):
            values = [exact_eval(hist, mp.mpf(mu), mp.mpf(2)**(-e))[0]
                      for e in (2, 3, 4)]
            assert values[0] < values[1] < values[2] < face
    print("PASS")


if __name__ == "__main__":
    main()
