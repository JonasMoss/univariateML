"""Verify uniform signs and the two endpoint comparisons of the real tie."""
import mpmath as mp

iv = mp.iv
iv.dps = 45
counts = (499*1024, 501*1024, 1)
p, q, w = (iv.mpf(count)/sum(counts) for count in counts)
target = iv.mpf("-.65")


def face(y, weights, k):
    mean = sum(pi*yi for pi, yi in zip(weights, y))
    terms = [pi*iv.exp(-k*yi) for pi, yi in zip(weights, y)]
    d = -iv.ln(sum(terms))
    ws = [k*yi-d for yi in y]
    value = iv.ln(k)-k*mean+d-1
    score = 1/k-mean+sum(yi*z for yi, z in zip(y, terms))/sum(terms)
    A = sum(pi*(u+u**2*(iv.exp(-u)-1)/2) for pi, u in zip(weights, ws))
    return value, score, A


def root_box(score, left, right):
    lo, hi = iv.mpf(left), iv.mpf(right)
    assert score(lo).a > 0 and score(hi).b < 0
    a, b = lo, hi
    for _ in range(27):
        mid = (a+b)/2
        if score(mid).a > 0:
            a = mid
        else:
            b = mid
    c, d = lo, hi
    for _ in range(27):
        mid = (c+d)/2
        if score(mid).b < 0:
            d = mid
        else:
            c = mid
    return iv.mpf([a, d])


def profile(b, t, y, weights):
    z = [t*iv.exp(b*yi) for yi in y]
    H = -sum(pi*iv.log1p(-iv.exp(-zi)) for pi, zi in zip(weights, z))
    R = (-(sum(pi*zi for pi, zi in zip(weights, z))) + H
         + iv.ln(t) + b*sum(pi*yi for pi, yi in zip(weights, y)))
    return iv.ln(b)-iv.ln(H)-1+R


def left_upper():
    # At largest level 1 the sample has two levels. Moving the largest
    # entry upwards decreases the fixed-b profile, by the scale score.
    y, weights = (0, 1), (p, q+w)
    k = iv.mpf("2.4")
    J, score, _ = face(y, weights, k)
    assert face(y, weights, iv.mpf("2.38"))[1].a > 0
    assert face(y, weights, iv.mpf("2.42"))[1].b < 0
    upper_I = J+iv.mpf(".02")*abs(score)
    # t >= 3: inverse-Weibull comparison in the report.
    h3 = -iv.log1p(-iv.exp(-3))
    assert (upper_I+(q+w-iv.mpf(".5"))+h3).b < target
    # t <= 1/1024, or b <= 1/8192 and t <= 3: crude H lower bound.
    assert (-iv.ln(p*iv.ln(1024))-1).b < target
    h_max = -iv.log1p(-iv.exp(-3*iv.exp(1)))
    assert (-iv.ln(8192*h_max)-1).b < target

    cells = [(1/8192, 1.0, 1/1024, 3.0)]
    visited = 0
    while cells:
        blo, bhi, tlo, thi = cells.pop()
        visited += 1
        assert visited < 100000
        B, T = iv.mpf([blo, bhi]), iv.mpf([tlo, thi])
        if profile(B, T, y, weights).b < target:
            continue
        if (bhi-blo)/blo >= (thi-tlo)/tlo:
            mid = (blo+bhi)/2
            cells.extend([(blo, mid, tlo, thi), (mid, bhi, tlo, thi)])
        else:
            mid = (tlo+thi)/2
            cells.extend([(blo, bhi, tlo, mid), (blo, bhi, mid, thi)])
    print("uniform left upper at deformation start", target, "cells", visited)


def check_uniform():
    weights = (p, q, w)
    lower = iv.inf
    for i in range(72):
        y = (0, 1, iv.mpf([2+i/4, 2+(i+1)/4]))
        K = root_box(lambda k: face(y, weights, k)[1], "2.38", "2.42")
        A = face(y, weights, K)[2]
        assert A.a > iv.mpf(".001")
        lower = min(lower, A.a)
    print("uniform inverse A lower", lower)

    # Fixed inverse-face trial lower-bounds every inverse optimum.
    I_lower = face((0, 1, 20), weights, iv.mpf("2.4"))[0]
    # At b=1, moving max y upwards decreases the optimized value.
    # The t box and its sign bounds are checked directly at v=2.
    def score(t):
        z = [t*iv.exp(yi) for yi in (0, 1, 2)]
        H = -sum(pi*iv.log1p(-iv.exp(-zi)) for pi, zi in zip(weights, z))
        R = sum(pi*zi/(iv.exp(zi)-1) for pi, zi in zip(weights, z))
        return (1-R+R/H)/t - (p+q*iv.exp(1)+w*iv.exp(2))

    T = root_box(score, "1.28", "1.30")
    trough = profile(1, T, (0, 1, 2), weights)
    P_max = -iv.ln(2*(1-w)-q)-1
    assert (I_lower-trough).a > iv.mpf(".01")
    assert (I_lower-P_max).a > iv.mpf(".6")
    print("uniform inverse lower minus trough upper", (I_lower-trough).a)
    print("uniform inverse lower minus power upper", (I_lower-P_max).a)

    start = profile(12, iv.mpf(2)/5*iv.exp(-12), (0, 1, 2), weights)
    assert start.a > target
    # The endpoint v=20 bounds all b>=1, split at log scale=2.
    assert P_max.b < I_lower.a
    assert iv.exp(-20).b < (w/4).a
    assert (w*iv.exp(18)/4).a > 1
    assert (-iv.ln(p)-w*iv.exp(18)/4-1).b < I_lower.a
    print("right trial at v=2", start)
    print("inverse lower", I_lower)


if __name__ == "__main__":
    left_upper()
    check_uniform()
