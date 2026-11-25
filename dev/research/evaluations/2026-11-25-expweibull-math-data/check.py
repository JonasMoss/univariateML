"""Reproduce the face crossing, the scalar profile, and its positivity witness."""
import mpmath as mp


def solve_decreasing(score, lo, hi):
    assert score(lo) > 0 and score(hi) < 0
    for _ in range(210):
        mid = (lo + hi) / 2
        if score(mid) > 0:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def inv_face(y, weights):
    mean = mp.fdot(y, weights)

    def score(k):
        terms = [p * mp.exp(-k * yi) for yi, p in zip(y, weights)]
        return 1/k - mean + mp.fdot(y, terms) / sum(terms)

    hi = mp.mpf(1)
    while score(hi) >= 0:
        hi *= 2
    k = solve_decreasing(score, mp.mpf("1e-7"), hi)
    d = -mp.log(mp.fsum(p * mp.exp(-k * yi) for yi, p in zip(y, weights)))
    w = [k * yi - d for yi in y]
    value = mp.log(k) - mp.fdot(weights, w) - 1
    correction = mp.fdot(weights, [v + v*v/2 * mp.expm1(-v) for v in w])
    return value, k, correction


def face_gap(p):
    return inv_face([0, 1], [p, 1-p])[0] + mp.log(p) + 1


def parts(z):
    if z < 1:
        return -mp.log(-mp.expm1(-z)), z/mp.expm1(z)
    return -mp.log1p(-mp.exp(-z)), z * mp.exp(-z)/(-mp.expm1(-z))


def profile(y, weights, b, t=None):
    r = [mp.exp(b * yi) for yi in y]

    def calc(u):
        z = [u * ri for ri in r]
        pairs = [parts(zi) for zi in z]
        H = mp.fdot(weights, [v[0] for v in pairs])
        R = mp.fdot(weights, [v[1] for v in pairs])
        return H, R

    def score(u):
        H, R = calc(u)
        return (1-R+R/H)/u - mp.fdot(weights, r)

    if t is None:
        hi = mp.mpf(1)
        while score(hi) >= 0:
            hi *= 2
        lo = hi/2
        while score(lo) <= 0:
            lo /= 2
        t = solve_decreasing(score, lo, hi)
    H, _ = calc(t)
    return mp.log(b*t) + b * mp.fdot(y, weights) - t * mp.fdot(weights, r) + H - mp.log(H) - 1, t, 1/H


def show(label, *values):
    print(label, *(mp.nstr(v, 22) for v in values))


def interval_witness():
    """Enclose the exact trial likelihood and the two optimized faces."""
    iv = mp.iv
    iv.dps = 50
    y = [iv.mpf(v)/5 for v in (0, 1, 2, 3, 5)]
    mean = sum(y)/5
    P = -iv.ln(1-mean)-1

    def inverse(k):
        k = iv.mpf(k)
        e = [iv.exp(-k*yi) for yi in y]
        return iv.ln(k)-k*mean-iv.ln(sum(e)/5)-1, 1/k-mean+sum(
            yi*ei for yi, ei in zip(y, e))/sum(e)

    assert inverse("3.53")[1].a > 0
    assert inverse("3.55")[1].b < 0
    mid, score = inverse("3.54")
    I_upper = mid + abs(score)/100
    b, t = iv.mpf(".1178"), iv.mpf("28.68")
    z = [t*iv.exp(b*yi) for yi in y]
    H = -sum(iv.ln(1-iv.exp(-zi)) for zi in z)/5
    ll = iv.ln(b*t)+b*mean-sum(z)/5+H-iv.ln(H)-1
    assert (ll-P).a > iv.mpf(".0005")
    assert (ll-I_upper).a > iv.mpf(".0005")
    print("interval certified trial minus power:", ll-P)
    print("interval certified trial minus inverse upper:", ll-I_upper)


def nested_check():
    d = mp.acosh(mp.e/2)
    den = 2**52
    lo = int(mp.floor(den*d))
    for count in (lo, lo+1):
        delta = mp.mpf(count)/den
        score = 1 + (mp.log1p(-mp.exp(-(1-delta)))
                     + mp.log1p(-mp.exp(-(1+delta))))/2
        assert score > 0 if count == lo else score < 0
        show(f"held shape 1, Weibull exponent score at delta={count}/{den}", score)


def balanced_inverse_check():
    z = mp.findroot(lambda v: v*mp.tanh(v)-1, (1, 2))
    d = mp.log(mp.cosh(z))
    I = mp.log(2*z)-d-1
    for T in (100, 1000, 10000):
        def likelihood(v, u):
            w = (u-v, u+v)
            t = [T*mp.exp(wi/T) for wi in w]
            h = [-mp.log1p(-mp.exp(-ti)) for ti in t]
            return (mp.log(2*v)+sum(w)/(2*T)
                    -sum(ti-T for ti in t)/2-mp.expm1(T)*sum(h)/2)

        v, u = mp.findroot(
            lambda v, u: (mp.diff(lambda x: likelihood(x, u), v),
                          mp.diff(lambda x: likelihood(v, x), u)), (z, d))
        scaled = T**2*(likelihood(v, u)-I)
        assert scaled < 0
        show(f"balanced inverse T={T}, scaled gap, limit", scaled, -z*z/6)
    assert abs(scaled+z*z/6) < mp.mpf(".001")


def main():
    mp.mp.dps = 65
    interval_witness()
    nested_check()
    balanced_inverse_check()
    p0 = mp.findroot(face_gap, (mp.mpf(".64"), mp.mpf(".65")))
    show("face crossing", p0)
    den = 2**53
    lo = int(mp.floor(p0 * den))
    for count in (lo, lo + 1):
        gap = face_gap(mp.mpf(count)/den)
        assert gap < 0 if count == lo else gap > 0
        show(f"face gap at {count}/{den}", gap)
    for label, p in (("one third", mp.mpf(1)/3), ("one half", mp.mpf(1)/2),
                     ("two thirds", mp.mpf(2)/3)):
        I, k, A = inv_face([0, 1], [p, 1-p])
        assert abs(A - k*(mp.mpf(".5")-p)) < mp.mpf("1e-59")
        show(label + " gap, k, A", I + mp.log(p)+1, k, A)

    y = [mp.mpf(v)/5 for v in (0, 1, 2, 3, 5)]
    weights = [mp.mpf(1)/5]*5
    P = -mp.log(1-mp.fdot(y, weights))-1
    I, k, A = inv_face(y, weights)
    show("dyadic sample P, I, k, A", P, I, k, A)
    for j in range(-12, 17):
        b = mp.power(2, mp.mpf(j)/4)
        ll, t, c = profile(y, weights, b)
        show("dyadic grid b, min hazard, c, excess", b, t, c, ll-max(P, I))
    for p in (mp.mpf(1)/3, mp.mpf(1)/2, p0, mp.mpf(2)/3):
        I, _, _ = inv_face([0, 1], [p, 1-p])
        P = -mp.log(p)-1
        values = []
        for j in range(-16, 33):
            b = mp.power(2, mp.mpf(j)/4)
            ll, _, _ = profile([0, 1], [p, 1-p], b)
            values.append(ll-max(P, I))
        show("two levels p, max grid excess, b at grid max",
             p, max(values), mp.power(2, mp.mpf(values.index(max(values))-16)/4))
        assert max(values) < 0


if __name__ == "__main__":
    main()
