"""Exact sign proof for the symmetric three-scale sample, with 80-digit diagnostics."""
from fractions import Fraction as F
import json
import mpmath as mp

RADII = [F(i, 100) for i in (4, 80, 5, 15, 33)] + [F(300)]
COUNTS = [300000, 4000000, 4000000, 4800000, 3100000, 1]
SHAPES = [2, 3, 4, 6, 10, 20, 50, 100]


def log_unit(x, terms):
    u = (x - 1) / (x + 1)
    square = u * u
    power = u
    total = F(0)
    for j in range(terms):
        total += power / (2*j + 1)
        power *= square
    low = 2 * total
    return low, low + 2 * power / ((2*terms + 1) * (1 - square))


def log_range(x, terms=18):
    shift = 0
    while x > 2:
        x /= 2
        shift += 1
    while x < 1:
        x *= 2
        shift -= 1
    lo, hi = log_unit(x, terms)
    a, b = log_unit(F(2), terms)
    ends = sorted((shift*a, shift*b))
    return lo + ends[0], hi + ends[1]


def scale(v, squares, counts, steps):
    lo, hi = F(0), max(squares)
    for _ in range(steps):
        mid = (lo + hi) / 2
        score = (v+1)*sum(w*q/(v*mid+q) for q,w in zip(squares, counts))
        if score > sum(counts):
            lo = mid
        else:
            hi = mid
    return lo, hi


def digamma_difference(v):
    # D(1)=2 log 2 and D(v+1)=2/v-D(v).
    a, b = 0, F(2)
    for j in range(1, v):
        a, b = F(2, j)-a, -b
    return a, b


def score(v, radii=RADII, counts=COUNTS):
    q = [r*r for r in radii]
    lo, hi = scale(v, q, counts, 75)
    a, b = digamma_difference(v)
    log_two = log_range(F(2))
    result = [a+j-1/F(v) for j in sorted(b*t for t in log_two)]
    for p, w in zip(q, counts):
        l, u = p/(v*hi), p/(v*lo)
        l_log = log_range(1+l)[0]
        u_log = log_range(1+u)[1]
        result[0] += F(w, sum(counts))*(-u_log+(1+1/F(v))*l/(1+l))
        result[1] += F(w, sum(counts))*(-l_log+(1+1/F(v))*u/(1+u))
    return result


def real(x):
    return mp.mpf(x.numerator)/x.denominator


if __name__ == '__main__':
    mp.mp.dps = 80
    results = {}
    for v in SHAPES:
        bounds = score(v)
        results[v] = [mp.nstr(real(x), 20) for x in bounds]
    print(json.dumps(results, indent=2))
