"""Interval certificates and numerical illustrations for the two exact ties."""
from mpmath import iv, mp

iv.dps = 50
mp.dps = 60
I = iv.mpf
one = I(1)


def box(lo, hi):
    return I([lo.a, hi.b])


def soft(z):
    return iv.ln(1 + iv.exp(z))


def sigmoid(z):
    return 1 / (1 + iv.exp(-z))


def held(a, r):
    distances = [I(".001"), r]
    value = 2 * iv.ln(a)
    score = 2 / a
    curvature = -2 / a**2
    for d in distances:
        z = -a * d
        p = sigmoid(z)
        value += (z - (1+a) * soft(z)) / 2
        score += (-d - soft(z) + (1+a)*d*p) / 2
        curvature += d*p*(2 - (1+a)*d*(1-p)) / 2
    return value, score, curvature


def held_cover(r):
    # A stationary point outside the root boxes must lose to a=3.
    witness = held(I(3), r)[0]
    pending = [(1, 2, 0), (4, 6500, 0), (10000, 20000, 0)]
    count = 0
    while pending:
        lo, hi, depth = pending.pop()
        a = I([lo, hi])
        value, score, _ = held(a, r)
        count += 1
        if value.b < witness.a or score.a > 0 or score.b < 0:
            continue
        assert depth < 30, (lo, hi, r, value, score)
        mid = (lo+hi) / 2
        pending.extend([(lo, mid, depth+1), (mid, hi, depth+1)])
    return count


def held_root(lo, hi, r):
    lo, hi = mp.mpf(lo), mp.mpf(hi)
    for _ in range(85):
        mid = (lo+hi)/2
        score = held(I(mid), r)[1]
        if score.a > 0:
            lo = mid
        else:
            assert score.b < 0
            hi = mid
    return I([lo, hi])


def check_held():
    rlo, rhi = I(".003"), I(".005")
    r = box(rlo, rhi)
    d = I(".001")
    assert 2 - I(".005") - iv.ln(2) > 0
    assert 2/I(20000) - I(".002") + I(20001)*d*iv.exp(-20) < 0
    assert held(I([2, 4]), r)[2].b < 0
    assert 2 - I(6501)*d*(1-sigmoid(-6500*d)) < 0
    for alo, ahi in ((2, 4), (6500, 10000)):
        assert held(I(alo), r)[1].a > 0
        assert held(I(ahi), r)[1].b < 0
    assert 3*sigmoid(-4*rhi) > 1
    assert I(10001)*sigmoid(-6500*rlo) < 1
    cells = 0
    for i in range(40):
        left = rlo + i*(rhi-rlo)/40
        right = rlo + (i+1)*(rhi-rlo)/40
        cells += held_cover(box(left, right))
    print("held compact cover cells:", cells)
    for s, sign in [(".003", 1), (".005", -1)]:
        r = I(s)
        a = held_root(2, 4, r)
        b = held_root(6500, 10000, r)
        diff = held(b, r)[0] - held(a, r)[0]
        assert sign*diff > 0
        print(f"held r={s}, root intervals {a}, {b}, high-minus-low {diff}")


eps = one/10000
w = 1-eps
error = one/500
klo, khi = 7*one/5, 8*one/5


def low_peak(k):
    return -iv.ln(w*k)-1


def high_peak(k):
    return 2*iv.ln(2/k)-2


def check_joint():
    k = box(klo, khi)
    low_left, low_right = 3*one/4, 9*one/10
    high_left, high_right = 11*one/10, 17*one/10
    low = box(low_left, low_right)
    high = box(high_left, high_right)
    tlo, thi = 1/iv.sqrt(w*k), 2/k
    assert low_left < tlo and tlo < low_right
    assert high_left < thi and thi < high_right
    assert low_right < 1/w and 1/w < high_left
    assert 2*eps*(1+iv.ln((1+2*eps)/(2*eps))) < error
    # The branch-peak loss at each band edge is smallest at the K endpoint
    # placing the corresponding hard maximum closest to that edge.
    for t, end in [(low_left, khi), (low_right, klo)]:
        assert low_peak(end) - (2*iv.ln(t)-w*end*t**2) > error
    for t, end in [(high_left, khi), (high_right, klo)]:
        assert high_peak(end) - (2*iv.ln(t)-end*t) > error
    for end, sign in [(klo, 1), (khi, -1)]:
        diff = high_peak(end)-low_peak(end)
        assert sign*diff > error
        print("joint K:", end, "hard high-minus-low:", diff)
    print("joint rational band separation and endpoint ordering certified")

    # Each band has one strictly concave profiled peak. Tail softplus and
    # its first two derivatives are bounded by exp(-10000) in either chart.
    tiny = iv.exp(I(-10000))
    for t, anchor, lower in [(low, I([-3, 0]), True), (high, I([8, 12]), False)]:
        A = 1+eps*t
        speed = k/eps
        if lower:
            assert (anchor + k*t/eps).a > 10000
            left = eps*(1-w*t - A*sigmoid(anchor.a))
            right = eps*(1-w*t - A*sigmoid(anchor.b)) + A*w*tiny
            tt = -2/t**2-2*k*w+2*eps*w*speed*tiny
            tb_abs = eps + w*(eps+A*speed)*tiny
            bb_abs_min = eps*sigmoid(anchor.a)*(1-sigmoid(anchor.b))
        else:
            assert (k*t/eps - anchor).a > 10000
            left = 1-A*w*sigmoid(anchor.a)-A*eps*tiny
            right = 1-A*w*sigmoid(anchor.b)
            tt = -2/t**2+2*eps**2*speed*tiny
            tb_abs = eps*w + eps*(eps+A*speed)*tiny
            bb_abs_min = w*sigmoid(anchor.a)*(1-sigmoid(anchor.b))
        assert left.a > 0 and right.b < 0
        assert (tt+tb_abs**2/bb_abs_min).b < 0
    assert 1 - (1+eps*high)*tiny > I(".99")
    assert high.a * I(".99") > low.b
    print("joint anchor enclosures, strict curvature and tie monotonicity certified")


def held_numerical():
    def ll(a, r):
        return 2*mp.log(a) + sum((-a*d-(1+a)*mp.log1p(mp.exp(-a*d)))/2
                                  for d in [mp.mpf(".001"), r])

    def equations(a, b, r):
        return (mp.diff(lambda v: ll(v, r), a),
                mp.diff(lambda v: ll(v, r), b), ll(b, r)-ll(a, r))

    a, b, r = mp.findroot(equations, (3, 8000, mp.mpf(".0034")))
    print("held numerical tie (r, low shape, high shape):",
          *(mp.nstr(x, 28) for x in (r, a, b)))


def joint_numerical():
    e = mp.mpf(1)/10000
    w = 1-e

    def ll(t, b, k, lower):
        a = e*t
        D = k*t/e
        if lower:
            return (2*mp.log(t)-k*w*t*t + e*((1-w*t)*b-(1+a)*mp.log1p(mp.exp(b)))
                    - (1+a)*w*mp.log1p(mp.exp(-b-D)))
        return (2*mp.log(t)-k*t+b-(1+a)*w*(b+mp.log1p(mp.exp(-b)))
                - (1+a)*e*mp.log1p(mp.exp(b-D)))

    def equations(t, b, u, c, k):
        return (mp.diff(lambda v: ll(v,b,k,True), t),
                mp.diff(lambda v: ll(t,v,k,True), b),
                mp.diff(lambda v: ll(v,c,k,False), u),
                mp.diff(lambda v: ll(u,v,k,False), c),
                ll(u,c,k,False)-ll(t,b,k,True))

    t, b, u, c, k = mp.findroot(equations, (mp.mpf(".82"), -1.5,
                                           mp.mpf("1.36"), 10, mp.mpf("1.47")))
    print("joint numerical tie (K, low shape, high shape):",
          *(mp.nstr(x, 28) for x in (k, e*t, e*u)))


if __name__ == "__main__":
    check_joint()
    check_held()
    joint_numerical()
    held_numerical()
