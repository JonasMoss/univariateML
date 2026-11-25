"""Prove a trough by interval evaluation at one normalized outer shape."""
import mpmath as mp


def check(count, total, trial_k, min_gap):
    iv = mp.iv
    iv.dps = 50
    p, q = iv.mpf(count)/total, iv.mpf(total-count)/total
    e = iv.exp(1)
    mean_r = p+q*e

    def value_score(t):
        z = (t, e*t)
        weights = (p, q)
        H = -sum(w*iv.ln(1-iv.exp(-u)) for w, u in zip(weights, z))
        R = sum(w*u/(iv.exp(u)-1) for w, u in zip(weights, z))
        return iv.ln(t)+q-t*mean_r+H-iv.ln(H)-1, (1-R+R/H)/t-mean_r

    lo, hi = iv.mpf(1)/1024, iv.mpf(16)
    assert value_score(lo)[1].a > 0
    assert value_score(hi)[1].b < 0
    for _ in range(35):
        mid = (lo+hi)/2
        score = value_score(mid)[1]
        if score.a > 0:
            lo = mid
        else:
            assert score.b < 0
            hi = mid
    upper = value_score(iv.mpf([lo, hi]))[0].b
    power = -iv.ln(p)-1
    k = iv.mpf(trial_k)
    inverse_trial = iv.ln(k)-k*q-iv.ln(p+q*iv.exp(-k))-1
    assert (power-upper).a > iv.mpf(min_gap)
    assert (inverse_trial-upper).a > iv.mpf(min_gap)
    print(f"lower share {count}/{total}")
    print("root bracket:", iv.mpf([lo, hi]))
    print("power minus upper:", power-upper)
    print("inverse trial minus upper:", inverse_trial-upper)


def separated_maxima():
    iv = mp.iv
    iv.dps = 60
    y = (0, 1, 2)
    counts = (499*1024, 501*1024, 1)
    weights = [iv.mpf(c)/sum(counts) for c in counts]
    mean = sum(p*v for p, v in zip(weights, y))

    def shape_value_score(b, t):
        r = [iv.exp(b*v) for v in y]
        z = [t*ri for ri in r]
        H = -sum(p*iv.ln(1-iv.exp(-zi)) for p, zi in zip(weights, z))
        R = sum(p*zi/(iv.exp(zi)-1) for p, zi in zip(weights, z))
        W = sum(p*ri for p, ri in zip(weights, r))
        return iv.ln(b*t)+b*mean-t*W+H-iv.ln(H)-1, (1-R+R/H)/t-W

    def inverse(k):
        terms = [p*iv.exp(-k*v) for p, v in zip(weights, y)]
        d = -iv.ln(sum(terms))
        w = [k*v-d for v in y]
        value = iv.ln(k)-sum(p*wi for p, wi in zip(weights, w))-1
        score = 1/k-mean+sum(v*z for v, z in zip(y, terms))/sum(terms)
        A = sum(p*(wi+wi**2*(iv.exp(-wi)-1)/2)
                for p, wi in zip(weights, w))
        return value, score, A

    def bracket(score, lo, hi):
        assert score(lo).a > 0 and score(hi).b < 0
        for _ in range(41):
            mid = (lo+hi)/2
            if score(mid).a > 0:
                lo = mid
            else:
                assert score(mid).b < 0
                hi = mid
        return iv.mpf([lo, hi])

    k = bracket(lambda k: inverse(k)[1], iv.mpf(2), iv.mpf(3))
    I, _, A = inverse(k)
    t = bracket(lambda t: shape_value_score(1, t)[1], iv.mpf(1)/1024, iv.mpf(16))
    valley, _ = shape_value_score(1, t)
    # Evaluate the high observation via log1p to preserve its exponentially
    # small H contribution when t*exp(24) is about 65000.
    b = 12
    t_hi = iv.mpf(2)/5 * iv.exp(-12)
    z = [t_hi*iv.exp(b*v) for v in y]
    W = sum(p*zi for p, zi in zip(weights, z))
    H = -sum(p*iv.log1p(-iv.exp(-zi)) for p, zi in zip(weights, z))
    right = iv.ln(b*t_hi)+b*mean-W+H-iv.ln(H)-1
    P = -iv.ln(2-mean)-1
    assert A.a > iv.mpf(".002")
    assert (I-valley).a > iv.mpf(".001")
    assert (right-valley).a > iv.mpf(".001")
    assert (right-P).a > iv.mpf(".001")
    assert (right-I).a > iv.mpf(".001")
    for label, v in (("A", A), ("I minus valley", I-valley),
                     ("right minus valley", right-valley),
                     ("right minus P", right-P), ("right minus I", right-I)):
        print("three levels", label, v)


if __name__ == "__main__":
    check(9, 14, "3", ".02")
    check(499, 1000, "2.4", ".001")
    separated_maxima()
