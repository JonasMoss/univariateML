"""Directed fixed-point enclosures for the two-level sample proof.

An integer endpoint represents an exact multiple of 2^-160. All interval
operations use integer arithmetic, including the exponential and logarithm
series with their remainder bounds.
"""
from dataclasses import dataclass
from fractions import Fraction
from functools import lru_cache
from math import isqrt

SCALE = 1 << 160
TERMS = 48


def ceildiv(n, d):
    return -((-n) // d)


@dataclass(frozen=True)
class Interval:
    lo: int
    hi: int

    def __add__(self, other):
        other = iv(other)
        return Interval(self.lo + other.lo, self.hi + other.hi)

    __radd__ = __add__

    def __neg__(self):
        return Interval(-self.hi, -self.lo)

    def __sub__(self, other):
        return self + -iv(other)

    def __rsub__(self, other):
        return iv(other) + -self

    def __mul__(self, other):
        other = iv(other)
        values = [a * b for a in (self.lo, self.hi) for b in (other.lo, other.hi)]
        return Interval(min(values) // SCALE, ceildiv(max(values), SCALE))

    __rmul__ = __mul__

    def __truediv__(self, other):
        other = iv(other)
        if other.hi < 0:
            return -self / -other
        assert other.lo > 0
        return self * Interval(SCALE**2 // other.hi, ceildiv(SCALE**2, other.lo))

    def __rtruediv__(self, other):
        return iv(other) / self

    def sqrt(self):
        assert self.lo >= 0
        low = isqrt(self.lo * SCALE)
        high = isqrt(self.hi * SCALE)
        return Interval(low, high + (high**2 != self.hi * SCALE))

    def bounds(self):
        return (self.lo / SCALE, self.hi / SCALE)


def iv(value):
    if isinstance(value, Interval):
        return value
    value = Fraction(value) * SCALE
    return Interval(value.numerator // value.denominator,
                    ceildiv(value.numerator, value.denominator))


def box(lo, hi):
    return Interval(iv(lo).lo, iv(hi).hi)


@lru_cache(maxsize=32768)
def exp_negative(z):
    """Enclose exp(-z), z >= 0, with a decreasing alternating series."""
    assert z.lo >= 0
    if z.lo >= 128 * SCALE:
        # exp(-128) < 2^-160 (log 2 < 4/5).
        return Interval(0, 1)
    if z.lo != z.hi:
        return Interval(exp_negative(Interval(z.hi, z.hi)).lo,
                        exp_negative(Interval(z.lo, z.lo)).hi)
    shifts = max(0, z.hi.bit_length() - SCALE.bit_length() + 1)
    reduced = z / (1 << shifts)
    term = total = iv(1)
    for k in range(1, TERMS + 1):
        term = -term * reduced / k
        total += term
    next_term = -term * reduced / (TERMS + 1)
    result = Interval((total + next_term).lo, total.hi)
    for _ in range(shifts):
        result *= result
    return result


def log_near_one(z):
    """For 1 <= z <= 2 use the odd-power atanh expansion."""
    assert SCALE <= z.lo <= z.hi <= 2 * SCALE
    w = (z - 1) / (z + 1)
    power = w
    square = w * w
    total = iv(0)
    for j in range(TERMS):
        total += power / (2 * j + 1)
        power *= square
    tail = 2 * power / ((2 * TERMS + 1) * (1 - square))
    return 2 * total + Interval(0, tail.hi)


LOG2 = log_near_one(iv(2))


def log(z):
    z = iv(z)
    assert z.lo > 0
    if z.lo != z.hi:
        return Interval(log(Interval(z.lo, z.lo)).lo, log(Interval(z.hi, z.hi)).hi)
    shifts = z.lo.bit_length() - SCALE.bit_length()
    reduced = z / (1 << shifts) if shifts >= 0 else z * (1 << -shifts)
    return shifts * LOG2 + log_near_one(reduced)


def two_level(b, m):
    return exp_negative(b), exp_negative(b * m)


def face_score(m):
    b0 = 10 / (9 + m)
    v, w = two_level(b0, m)
    return 10 - 2 * (9 * v + w)


def difference(b, eta, m):
    v, w = two_level(b, m)
    return (10 * log(b * (9 + m) / 10) - b * (9 + m) + 10
            + 9 * log(1 + eta * (1 - v)) + log(1 + eta * (1 - w))
            - eta * (9 * v + w))


@lru_cache(maxsize=8192)
def point_shape(b, m):
    v, w = two_level(b, m)
    a, c, v_sum = 1 - v, 1 - w, 9 * v + w
    constant = v_sum - 9 * a - c
    if constant.lo >= 0:
        return iv(0)
    linear = v_sum * (a + c) - 10 * a * c
    quadratic = v_sum * a * c
    includes_face = constant.hi >= 0
    constant = Interval(constant.lo, min(0, constant.hi))
    discriminant = linear * linear - 4 * quadratic * constant
    root = (discriminant.sqrt() - linear) / (2 * quadratic)
    return Interval(0 if includes_face else max(0, root.lo), max(0, root.hi))


def shape_box(b, m):
    # The shape score is increasing in each of b and M at fixed eta.
    lower = point_shape(Interval(b.lo, b.lo), Interval(m.lo, m.lo))
    upper = point_shape(Interval(b.hi, b.hi), Interval(m.hi, m.hi))
    return Interval(lower.lo, upper.hi)


def derivatives(b, m):
    v, w = two_level(b, m)
    a, c = 1 - v, 1 - w
    eta = shape_box(b, m)
    d, e = 1 + eta * a, 1 + eta * c
    score = 10 / b - 9 - m + eta * (9 * v * (1 + 1 / d) + m * w * (1 + 1 / e))
    cross = 9 * v * (1 + 1 / (d * d)) + m * w * (1 + 1 / (e * e))
    den = (a / d) * (a / d) * 9 + (c / e) * (c / e)
    negative = (10 / (b * b)
                + eta * (9 * v * (1 + (1 + eta) / (d * d))
                         + m * m * w * (1 + (1 + eta) / (e * e))))
    return score, cross * cross / den - negative


def uniformly_signed(b, m, index, direction):
    stack = [(b, m, 0)]
    accepted = 0
    while stack:
        b, m, depth = stack.pop()
        value = derivatives(b, m)[index] * direction
        if value.lo > 0:
            accepted += 1
            continue
        assert value.hi > 0 and depth < 24, (b.bounds(), m.bounds(), value.bounds())
        if 4 * (b.hi - b.lo) >= m.hi - m.lo and b.hi - b.lo > 1:
            mid = (b.lo + b.hi) // 2
            stack.extend([(Interval(b.lo, mid), m, depth + 1),
                          (Interval(mid, b.hi), m, depth + 1)])
        else:
            mid = (m.lo + m.hi) // 2
            stack.extend([(b, Interval(m.lo, mid), depth + 1),
                          (b, Interval(mid, m.hi), depth + 1)])
    return accepted


def positive_peak(m):
    lo, hi = iv("9/10").lo, iv("21/20").hi
    for _ in range(95):
        mid = (lo + hi) // 2
        score = derivatives(Interval(mid, mid), m)[0]
        assert score.lo > 0 or score.hi < 0
        if score.lo > 0:
            lo = mid
        else:
            hi = mid
    b = Interval(lo, hi)
    return difference(b, shape_box(b, m), m)


def main():
    assert face_score(iv(8)).hi < 0
    assert difference(iv(1), iv(1), iv(8)).lo > 0
    assert LOG2.hi < iv("4/5").lo
    assert (9 * exp_negative(iv("1/2")) - 5).lo > 0
    # Uniform compact upper endpoint on [8,128].
    assert (10 * log(20) + 10 * log(10) - 140 - 10 * log(iv("10/137")) + 10).hi < 0
    # At M=128, the upper envelope decreases on [1/2, infinity).
    assert (10 * log(iv("1/2")) + 10 * log(10) - iv("127/2")
            - 10 * log(iv("10/137")) + 10).hi < 0
    print("M=8 inward score:", face_score(iv(8)).bounds())
    print("M=8 gain at (1,1):", difference(iv(1), iv(1), iv(8)).bounds())
    m = box("8.60", "8.62")
    v, w = two_level(iv(".58"), iv("8.62"))
    assert (9 * v + w - 5).lo > 0   # beta > .58 > b0
    v, w = two_level(iv(".63"), iv("8.60"))
    assert (9 * v + w - 5).hi < 0   # beta < .63
    regions = [(".58", ".63", 0, -1), (".63", ".76", 1, 1),
               (".76", ".90", 0, 1), (".90", "1.05", 1, -1),
               ("1.05", "20", 0, -1)]
    counts = [uniformly_signed(box(l, h), m, i, s) for l, h, i, s in regions]
    print("Exact sign-cover cell counts:", counts)
    for value, sign in (("8.60", 1), ("8.62", -1)):
        gain = positive_peak(iv(value))
        assert (sign * gain).lo > 0
        print("M, exact gain bounds:", value, gain.bounds())
    # Constants in the derivative bound for M >= 8.
    assert exp_negative(iv("7/10")).lo > iv("49/100").hi
    assert exp_negative(iv(4)).hi < iv("1/50").lo
    assert exp_negative(iv("49/10")).hi < iv("1/100").lo
    assert (iv("10/17") - iv("7/10") * (1 - iv("1/45"))).hi < 0
    for token, sign in (("0x1.139be516e77b3p+3", 1), ("0x1.139be516e77b4p+3", -1)):
        gain = positive_peak(iv(float.fromhex(token)))
        assert (sign * gain).lo > 0
        print("Dyadic, exact gain bounds:", token, gain.bounds())
    # Exact real tangency with positive right curvature.
    assert face_score(iv("7.95")).lo > 0 > face_score(iv("7.97")).hi
    m = box("7.95", "7.97")
    b = 10 / (9 + m)
    v, w = two_level(b, m)
    cross = 2 * (9 * v + m * w)
    den = 9 * (1 - v) * (1 - v) + (1 - w) * (1 - w)
    assert (cross * cross / den - 10 / (b * b)).lo > 0
    print("Seam crossing has strictly positive right curvature.")
    # Opposite curvature example with sample (0,1,M).
    assert (1 + 2 * exp_negative(iv("3/2")) - iv("3/2")).hi < 0
    assert (1 + exp_negative(iv(1)) + exp_negative(iv(2)) - iv("3/2")).lo > 0
    print("Exact interval checks passed.")


if __name__ == "__main__":
    main()
