"""Exact polynomial cuts and interval sign/height checks on rational inputs."""

from collections import Counter

import mpmath as mp
import sympy as sp

mp.iv.dps = 65
iv = mp.iv


def interval(q):
    q = sp.Rational(q)
    return iv.mpf(int(q.p)) / int(q.q)


def profile(data, rate):
    n = len(data)
    a = sum(iv.ln(1 + rate * u) for u in data) / n
    q = sum(1 / (1 + rate * u) for u in data) / n
    return a - (1 - q) / q, iv.ln(rate * sum(data) / n / a) - a


def sign(x):
    if x.a > 0:
        return 1
    if x.b < 0:
        return -1
    raise ArithmeticError(f"sign unresolved: {x}")


def check(data, expected):
    r = sp.Symbol("r")
    n = len(data)
    weights = Counter(data)
    d = sp.prod(1 + r * u for u in weights)
    numerator = sum(sp.Rational(w, n) * sp.div(d, 1 + r * u, r)[0]
                    for u, w in weights.items())
    polynomial = sp.Poly((d - numerator) * numerator**2 -
                         r * d * (numerator * sp.diff(d, r) - sp.diff(numerator, r) * d), r)
    cuts = []
    for (lo, hi), _ in polynomial.intervals(eps=sp.Rational(1, 10**35), inf=0):
        if hi == 0:
            continue
        assert lo > 0
        lo_iv, hi_iv = interval(lo), interval(hi)
        box = iv.mpf([lo_iv.a, hi_iv.b])
        cuts.append((lo, hi, sign(profile(data, box)[0])))

    # H tends to -infinity. The final critical point (if any) is followed
    # by a strictly decreasing segment, so we can safely expand the bracket.
    high = max(1, 2 * cuts[-1][1]) if cuts else sp.Integer(1)
    while profile(data, interval(high))[0].b >= 0:
        high *= 2

    roots = []
    for left, right in zip(cuts, cuts[1:] + [(high, high, -1)]):
        if left[2] == right[2]:
            continue
        lo, hi = left[1], right[0]
        for _ in range(100):
            mid = (lo + hi) / 2
            if sign(profile(data, interval(mid))[0]) == left[2]:
                lo = mid
            else:
                hi = mid
        bounds = iv.mpf([interval(lo).a, interval(hi).b])
        roots.append((left[2], sign(profile(data, bounds)[1])))

    print("powered data", data, "positive critical cuts", len(cuts),
          "stationary (left score sign, face-gap sign)", roots)
    assert tuple(roots) == expected


if __name__ == "__main__":
    check((1, 2), ())
    check((1, 100), ((-1, -1), (1, 1)))
    check((1, 3000, 3000, 22000), ((1, 1), (-1, 1), (1, 1)))
