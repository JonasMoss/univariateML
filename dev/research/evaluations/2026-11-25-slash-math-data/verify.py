"""Exact witness checks and 80-digit diagnostics for the slash report."""

from collections import Counter
from fractions import Fraction
from math import factorial, prod

import mpmath as mp
import sympy as sp


def lower(t):
    """Rational lower bound on 1 - exp(-t) from Taylor's theorem."""
    if t >= 20:
        return 1 - 1 / sum(Fraction(20)**h / factorial(h) for h in range(41))
    t = min(t, Fraction(4))
    return sum((-1)**(h + 1) * t**h / factorial(h) for h in range(1, 41))


def witness(residuals, m, s):
    k = sum(residuals.values())
    t = m**2 / (2*s**2)
    factors = []
    for d, count in residuals.items():
        off_t = (d-m)**2 / (2*s**2)
        factor = lower(off_t) * (d / (d-m))**2
        assert factor > 0
        factors.append(factor**count)
    assert sum(count / d for d, count in residuals.items()) == 0
    return (lower(t) / t)**k * prod(factors)


def a(t):
    return -mp.expm1(-t) / t if t else mp.mpf(1)


def log_ratio(residuals, m, s):
    k = sum(residuals.values())
    t = m**2 / (2*s**2)
    return k*mp.log(a(t)) + mp.fsum(
        count * mp.log(d**2 / (2*s**2) * a((d-m)**2 / (2*s**2)))
        for d, count in residuals.items()
    )


def profile_scale(residuals, m):
    k = sum(residuals.values())

    def score(s):
        def g(t):
            return 1 - 2*t/mp.expm1(t) if t else -1

        return k*g(m**2/(2*s**2)) + mp.fsum(
            count*g((d-m)**2/(2*s**2)) for d, count in residuals.items()
        )

    lo, hi = m/20, mp.mpf(20)
    assert score(lo) > 0 > score(hi)
    for _ in range(225):
        mid = (lo+hi)/2
        if score(mid) > 0:
            lo = mid
        else:
            hi = mid
    return (lo+hi)/2


def main():
    u, w = sp.symbols("u w")
    pair = (1+w)**2 * (1+2*u*(1-w)+u**2*(1+w)**2)
    assert sp.expand(pair - 4*w - (u*(1+w)**2-(w-1))**2) == 0

    old = {Fraction(-1): 1, Fraction(9): 9}
    assert witness(old, Fraction(4), Fraction(3)) > Fraction(1456, 1000)
    b = Fraction(300000, 10029)
    family = {Fraction(-1): 1, Fraction(-10000): 29, b: 30}
    m, s = b/5, 3*b/10
    assert witness(family, m, s) > 1
    print("Exact rational witnesses and symbolic pair identity passed.")

    mp.mp.dps = 80

    def mpq(q):
        return mp.mpf(q.numerator) / q.denominator

    for sample, mu, scale in ((old, Fraction(4), Fraction(3)), (family, m, s)):
        ratio = mp.exp(log_ratio({mpq(d): c for d, c in sample.items()},
                                 mpq(mu), mpq(scale)))
        print("Witness L/B:", mp.nstr(ratio, 18))

    residuals = Counter([mp.mpf(-1)] + [mp.mpf(9)]*9)
    for exponent in (6, 12, 24):
        delta = mp.mpf(10)**(-exponent)
        scale = profile_scale(residuals, delta)
        gap = log_ratio(residuals, delta, scale)
        predicted = -10*delta**2*mp.log(1/delta)
        second = 1 + mp.mpf(1)/9 - 5*(1 + mp.log(mp.mpf(1)/5))
        assert gap < 0 and abs(gap/predicted-1) < mp.mpf("0.2")
        assert abs((gap-predicted)/delta**2-second) < mp.mpf("0.002")
        print("delta=1e-", exponent, " profile gap / leading term = ",
              mp.nstr(gap/predicted, 12), ", second-order coefficient = ",
              mp.nstr((gap-predicted)/delta**2, 12), sep="")


if __name__ == "__main__":
    main()
