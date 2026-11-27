"""Exact t2 box cover and interval checks for the reflection example."""
from fractions import Fraction as Q
from math import comb
from mpmath import iv, mp

BULK = ((-Q(7, 4), 1), (-Q(1), 10), (Q(1), 10), (Q(7, 4), 1))
REPEATS = 2**9
REMOTE = 2**9
DF = 31
TWO_DF_BOUND = Q(23, 100)
SEPARATION = Q(233, 1000)

def box_cover():
    """Bound the squared likelihood of the 22-point bulk at shape two."""
    target = TWO_DF_BOUND**44
    stack = [(Q(0), Q(7, 4), Q(1, 32), Q(8), Q(0), Q(1), 0)]
    visited = 0
    max_depth = 0
    while stack:
        cl, cu, sl, su, wl, wu, depth = stack.pop()
        visited += 1
        max_depth = max(max_depth, depth)
        upper = 1 / sl**44
        for x, count in BULK:
            if x < cl:
                distance, fraction = cl - x, wu
            elif x > cu:
                distance, fraction = x - cu, 1 - wl
            else:
                continue
            upper /= (1 + (distance / (su * fraction))**2)**(3 * count)
        if upper < target:
            continue
        assert depth < 60
        low = [cl, sl, wl]
        high = [cu, su, wu]
        axis = max(range(3), key=lambda i: (high[i] - low[i]) /
                   (Q(7, 4), Q(8), Q(1))[i])
        middle = (low[axis] + high[axis]) / 2
        for lo, hi in ((low[axis], middle), (middle, high[axis])):
            bounds = list(zip(low, high))
            bounds[axis] = lo, hi
            stack.append((*[v for pair in bounds for v in pair], depth + 1))
    print(f"Exact t2 bulk cover: {visited} boxes, maximum depth {max_depth}")

def witness():
    iv.dps = 80
    mp.dps = 30
    n = 22 * REPEATS + 2
    a, b = iv.mpf(8) / 5, iv.mpf(2) / 5
    j = (DF - 1) // 2
    peak = iv.mpf(2 * 4**j) / (comb(2*j, j) * iv.pi * iv.sqrt(DF))
    moment = peak * DF / (DF - 1)
    z = -iv.mpf(3) / 5
    variance_factor = (iv.mpf(DF) / (DF - 2)) * (1 + 3*z*z) / 4 - moment**2 * z*z
    rescale = (iv.mpf(9) / 8) / (2 * iv.sqrt(variance_factor))
    a, b = a * rescale, b * rescale
    c = moment * (a - b)

    def log_density(x):
        y = iv.mpf(x.numerator) / x.denominator if isinstance(x, Q) else iv.mpf(x)
        residual = y - c
        assert residual.b < 0 or residual.a > 0
        side = a if residual.b < 0 else b
        return iv.log(peak / (a + b)) - (DF + 1) // 2 * iv.log(
            1 + (residual / side)**2 / DF)

    bulk = sum(count * log_density(x) for x, count in BULK)
    score = (REPEATS * bulk + log_density(-REMOTE) + log_density(REMOTE)) / n
    threshold = iv.log(iv.mpf(SEPARATION.numerator) / SEPARATION.denominator)
    t2 = (22 * REPEATS * iv.log(iv.mpf(TWO_DF_BOUND.numerator) / TWO_DF_BOUND.denominator) + 2 * iv.log(32)) / n
    t2_small = -iv.log(32) / 2
    t2_large = -iv.log(8)
    symmetric = (22 * REPEATS * (-iv.log(2 * iv.pi) / 2 - iv.mpf(1) / 2)
                 - 2 * REPEATS * iv.log(iv.mpf(7) / 4)) / n
    normal = iv.log(iv.sqrt(n) / REMOTE)
    half = iv.log(2) / n - iv.log(REMOTE - 2)
    assert score.a > threshold.b
    for bound in (t2, t2_small, t2_large, symmetric, normal, half):
        assert bound.b < threshold.a
    print("80-digit intervals (displayed to 16 digits)")
    for label, value in (("finite witness", score), ("threshold", threshold),
                         ("t2", t2), ("symmetric", symmetric),
                         ("normal", normal), ("half", half)):
        print(f"  {label:15} {mp.nstr(mp.mpf(value.mid), 16)}")

if __name__ == "__main__":
    box_cover()
    witness()
