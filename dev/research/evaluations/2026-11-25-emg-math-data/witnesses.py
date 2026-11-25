"""Rational profile enclosures and independent mpmath checks for TASK-346."""
from fractions import Fraction as F
from math import isqrt

import mpmath as mp
import sympy as s

SITES = list(range(-3, 4))
V = s.Matrix([[x**i for x in SITES] for i in range(7)])


def weights(epsilon, a, b, c):
    return [F(value) for value in V.inv()*s.Matrix([
        1, 0, 1, a*epsilon**3, 3+F(4, 3)*b*epsilon**2, F(5, 3)*c*epsilon, 15
    ])]


def mills(a):
    term = 1/a
    total = term
    for j in range(1, 10):
        term *= -F(2*j-1)/a**2
        total += term
    return total, total-term


def log_interval(x):
    w = (x-1)/(x+1)
    assert abs(w) < F(1, 16)
    scale = 2**128
    floor = F(w.numerator*scale//w.denominator, scale)
    ceil = floor+F(1, scale)
    error = F(1, 2**97)

    def series(z):
        return 2*sum(z**(2*j+1)/F(2*j+1) for j in range(12))

    return series(floor)-error, series(ceil)+error


def profile_bounds(w, q):
    k = 1/q
    scale = 2**80
    square = (1+q*q)*scale**2
    b = F(isqrt(square.numerator//square.denominator), scale)
    u = -q
    lower, upper = log_interval(b)
    lower += (1-b*b-u*u)/2
    upper += (1-b*b-u*u)/2
    # Gradient in (u,b). Interval endpoints bound the score at the center.
    gu, gb = [F(0), F(0)], [1/b, 1/b]
    for x, p in zip(SITES, w):
        z = b*x-u
        ml, mh = mills(k-z)
        lower += p*log_interval(k*ml)[0]
        upper += p*log_interval(k*mh)[1]
        sl, sh = 1/mh-k, 1/ml-k
        gu[0] -= p*sh
        gu[1] -= p*sl
        gb[0] += p*min(x*sl, x*sh)
        gb[1] += p*max(x*sl, x*sh)
    norm2 = max(map(abs, gu))**2+max(map(abs, gb))**2
    # On the half-unit ball, k-z >= k-(3*b+abs(u)+2) > 2.
    assert b > F(1, 2) and k-(3*b+abs(u)+2) > 2
    assert norm2 < F(1, 64)
    return lower, upper+norm2


def high_precision(w, q):
    def cast(a):
        return mp.mpf(a.numerator)/a.denominator

    k = 1/cast(q)
    p = [cast(v) for v in w]

    def terms(u, b):
        z = [b*x-u for x in SITES]
        m = [mp.sqrt(mp.pi/2)*mp.exp((k-v)**2/2)
             *mp.erfc((k-v)/mp.sqrt(2)) for v in z]
        return z, m, [1/v-k for v in m]

    def score(u, b):
        _, _, scores = terms(u, b)
        return (-sum(a*g for a, g in zip(p, scores)),
                1/b+sum(a*x*g for a, x, g in zip(p, SITES, scores)))

    u, b = mp.findroot(score, (-1/k, mp.sqrt(1+1/k**2)))
    z, m, _ = terms(u, b)
    return mp.log(b)+sum(a*((1-v*v)/2+mp.log(k*mj))
                         for a, v, mj in zip(p, z, m))


def main():
    from math import lcm

    epsilon = F(1, 1024)
    cases = [
        ("two peaks", (F(144, 23), F(-231, 23), F(126, 23)),
         [F(1, 2), F(1), F(36, 23), F(2), F(5, 2)]),
        ("zero skew and negative kurtosis", (0, -1, 3), [F(1)]),
        ("negative skew", (F(-1, 8), -1, 3), [F(1)]),
    ]
    for name, parameters, grid in cases:
        w = weights(epsilon, *parameters)
        assert min(w) > 0
        n = lcm(*(p.denominator for p in w))
        print(name, "counts", [int(p*n) for p in w])
        bounds = [profile_bounds(w, epsilon*u) for u in grid]
        if len(grid) == 5:
            assert bounds[1][0] > max(bounds[0][1], bounds[2][1], 0)
            assert bounds[3][0] > max(bounds[2][1], bounds[4][1], 0)
        else:
            assert bounds[0][0] > 0
        for precision in (70, 100):
            with mp.workdps(precision):
                values = []
                for u, (lo, hi) in zip(grid, bounds):
                    v = high_precision(w, epsilon*u)
                    assert mp.mpf(lo.numerator)/lo.denominator <= v
                    assert v <= mp.mpf(hi.numerator)/hi.denominator
                    values.append(mp.nstr(v, 16))
                print(precision, values)


if __name__ == "__main__":
    main()
