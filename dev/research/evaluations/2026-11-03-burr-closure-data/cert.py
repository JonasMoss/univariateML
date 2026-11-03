"""Small exact-rational enclosures, used only for the displayed witnesses.

Every arithmetic operation rounds OUT to rational multiples of 10^-45.
Taylor/atanh remainders are bounded explicitly; no binary float enters.
"""
from fractions import Fraction as F
from functools import lru_cache

Q = 10**45


def down(x):
    return F((x * Q).__floor__(), Q)


def up(x):
    return F((x * Q).__ceil__(), Q)


class I:
    def __init__(self, lo, hi=None):
        if isinstance(lo, I):
            self.lo, self.hi = lo.lo, lo.hi
        else:
            self.lo, self.hi = F(lo), F(lo if hi is None else hi)
        assert self.lo <= self.hi

    def __add__(self, other):
        o = I(other)
        return I(down(self.lo + o.lo), up(self.hi + o.hi))

    __radd__ = __add__

    def __neg__(self):
        return I(-self.hi, -self.lo)

    def __sub__(self, other):
        return self + -I(other)

    def __rsub__(self, other):
        return I(other) + -self

    def __mul__(self, other):
        o = I(other)
        v = [a * b for a in (self.lo, self.hi) for b in (o.lo, o.hi)]
        return I(down(min(v)), up(max(v)))

    __rmul__ = __mul__

    def __truediv__(self, other):
        o = I(other)
        assert o.lo * o.hi > 0
        return self * I(down(1 / o.hi), up(1 / o.lo))

    def __rtruediv__(self, other):
        return I(other) / self

    def pair(self):
        # Display only; assertions below use exact Fraction endpoints.
        return [float(self.lo), float(self.hi)]


@lru_cache(None)
def exp_point(t):
    t = F(t)
    if t < 0:
        return 1 / exp_point(-t)
    assert t <= 20
    term = I(1)
    total = term
    for j in range(1, 181):
        term = term * t / j
        total = total + term
    remainder = term * t / 181 / (1 - t / 182)
    return I(total.lo, up(total.hi + remainder.hi))


def exp(t):
    t = I(t)
    return I(exp_point(t.lo).lo, exp_point(t.hi).hi)


def log_series(t):
    assert 1 <= t <= 2
    z = I((t - 1) / (t + 1))
    term = z
    total = I(0)
    for j in range(60):
        total = total + 2 * term / (2 * j + 1)
        term = term * z * z
    remainder = 2 * term / 121 / (1 - z * z)
    return I(total.lo, up(total.hi + remainder.hi))


@lru_cache(None)
def log_point(t):
    t = F(t)
    assert t > 0
    exponent = 0
    while t < 1:
        t *= 2
        exponent -= 1
    while t > 2:
        t /= 2
        exponent += 1
    return log_series(t) + exponent * log_series(F(2))


def log(t):
    t = I(t)
    return I(log_point(t.lo).lo, log_point(t.hi).hi)


def weib(y, c):
    n = len(y)
    e = [exp(c * t) for t in y]
    total = sum(e, I(0))
    val = n * log(c) + c * sum(y, I(0)) - n * log(total / n) - n
    score = n / c + sum(y, I(0)) - n * sum((t * v for t, v in zip(y, e)), I(0)) / total
    v = [n * t / total for t in e]
    slope = sum((t * t / 2 for t in v), I(0)) - n
    return val, score, slope


def certify(y, c_lo, c_hi, finite_c, finite_a):
    y = [I(t) for t in y]
    n = len(y)
    lo, hi = I(c_lo), I(c_hi)
    val, score, _ = weib(y, lo)
    assert score.lo > 0
    assert weib(y, hi)[1].hi < 0
    upper = val + score * (hi - lo)
    w = I(val.lo, upper.hi)
    p = n * log(n / sum(y, I(0))) - n
    c, a = I(finite_c), I(finite_a)
    z = [a + c * t for t in y]
    finite = n * log(c) + sum(z, I(0)) - 2 * sum((log(1 + exp(t)) for t in z), I(0))
    slope = weib(y, I(c_lo, c_hi))[2]
    return {"W": w, "P": p, "finite": finite, "A": slope,
            "finite_minus_W": finite - w, "finite_minus_P": finite - p,
            "W_minus_P": w - p}


def second_order(y, c):
    y, c = [I(t) for t in y], I(c)
    n = len(y)
    e = [exp(c*t) for t in y]
    v = [n*t/sum(e, I(0)) for t in e]
    jcc = n/(c*c) + sum((t*t*u for t, u in zip(y, v)), I(0))
    jca = sum((t*u for t, u in zip(y, v)), I(0))
    dc = sum((t*u*(u-1) for t, u in zip(y, v)), I(0))
    de = sum((u*(u-1) for u in v), I(0))
    det = n*jcc-jca*jca
    assert det.lo > 0
    b = sum((u*u/2-u*u*u/3 for u in v), I(0))
    return b+(n*dc*dc-2*jca*dc*de+jcc*de*de)/(2*det)


def root_bracket(y):
    lo, hi = F(3), F(4)
    assert weib(y, I(lo))[1].lo > 0
    assert weib(y, I(hi))[1].hi < 0
    for _ in range(40):
        mid = (lo+hi)/2
        score = weib(y, I(mid))[1]
        if score.lo > 0:
            lo = mid
        else:
            assert score.hi < 0
            hi = mid
    return lo, hi


def certificates():
    y = [F(t, 15) for t in [0, 5, 7, 8, 8, 15, 15]]
    witness = certify(y, "3.31235", "3.31237", "5.16", "-2.816")
    assert witness["finite_minus_W"].lo > F(63, 1000)
    assert witness["finite_minus_P"].lo > F(47, 100)
    assert witness["A"].hi < -F(55, 1000)
    assert witness['W_minus_P'].lo > 0
    witness['U'] = 28*(1+log(I(14))-witness['W']/7)
    assert witness['U'].hi < 112
    # With gap delta=.06, K=(2*n/delta)*exp(2*U*R) < 10^100.
    assert (log(I(F(700, 3)))+224-100*log(I(10))).hi < 0
    witness['certified_log10_tail_cutoff'] = I(100)

    def seam_y(t):
        return [0, t, F(7, 15), F(8, 15), F(8, 15), 1, 1]

    left = certify(seam_y(F(".23826")), "3.2078", "3.2079", "4.932", "-2.624")
    right = certify(seam_y(F(".23827")), "3.2078", "3.2079", "4.932", "-2.624")
    assert left["W_minus_P"].hi < 0 < right["W_minus_P"].lo
    assert left['W_minus_P'].hi < -F(1, 50000)
    assert (28*(1+log(I(14))-left['P']/7)).hi < 114
    # Uniform tail cutoff for the majorization family in the report.
    assert (log(I(350000))+228-106*log(I(10))).hi < 0
    left['uniform_family_log10_tail_cutoff'] = I(106)
    whole = certify(seam_y(I(".23826", ".23827")), "3.2078", "3.2079", "4.932", "-2.624")
    assert whole["finite_minus_W"].lo > F(26, 1000)
    assert whole["finite_minus_P"].lo > F(26, 1000)
    assert whole["A"].hi < -F(87, 1000)

    # Positive slope at a globally Pareto-winning sample: (log x)=(0,0,1).
    two = certify([0, 0, 1], "2.1163630158", "2.1163630159", 1, 0)
    assert two["A"].lo > F(7, 1000)
    assert two["P"].lo > two["W"].hi

    # A zero of the first-order Weibull slope, with positive second order.
    def zero_y(t):
        return [I(a)*(1-t)+I(b)*t for a, b in zip(y, [0, 0, 0, 0, 0, 1, 1])]

    yl, yr = zero_y(I('.0439243843')), zero_y(I('.0439243846'))
    zero_left = certify(yl, *root_bracket(yl), 5, -3)
    zero_right = certify(yr, *root_bracket(yr), 5, -3)
    # Tighter nuisance brackets avoid interval cancellation in A near zero.
    # These are verified by the exact score signs in certify().
    assert zero_left['A'].hi < 0 < zero_right['A'].lo
    zero_whole_y = zero_y(I('.0439243843', '.0439243846'))
    zero_whole = certify(zero_whole_y, '3.2308225', '3.2308227', 5, -3)
    zero_whole['C'] = second_order(zero_whole_y, I('3.2308225', '3.2308227'))
    assert zero_whole['W_minus_P'].lo > F(11, 100)
    assert zero_whole['C'].lo > F(30, 100)
    return {name: {key: value.pair() for key, value in row.items()}
            for name, row in [("negative_slope_witness", witness),
                              ("seam_left", left), ("seam_right", right),
                              ("seam_whole_interval", whole),
                              ("positive_slope_nonexistence", two),
                              ("zero_slope_left", zero_left),
                              ("zero_slope_right", zero_right),
                              ("zero_slope_whole_interval", zero_whole)]}


if __name__ == "__main__":
    import json
    print(json.dumps(certificates(), indent=2))
