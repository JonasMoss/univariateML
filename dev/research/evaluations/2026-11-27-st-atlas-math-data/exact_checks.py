"""Exact arithmetic for the reflected and two-scale counterexamples."""

from fractions import Fraction as Q
from math import isqrt


def sqrt_bounds(value):
    """Rational bounds of width 2**-80 on a positive square root."""
    denominator = 2**80
    numerator = isqrt(value.numerator * denominator**2 // value.denominator)
    return Q(numerator, denominator), Q(numerator + 1, denominator)


def reflected():
    m = 9
    n = 2 * m + 2
    far = 2**128
    width = 1000
    scale_squared = Q(5, 4)
    location = Q(1, 2)
    slant = -Q(1, 2)
    ceiling = scale_squared ** (m + 1) / (1 + scale_squared) ** (2 * m)

    # Lower bound on R^4 pi^n L at the finite witness.
    witness = far**4 * scale_squared ** (n // 2)
    for point, count in [(-far, 1), (-1, m), (1, m), (far, 1)]:
        residual = point - location
        lower, upper = sqrt_bounds(scale_squared + (1 + slant**2) * residual**2)
        signed = slant * residual
        skew_lower = 1 + signed / (lower if signed < 0 else upper)
        assert skew_lower > 0
        witness *= (skew_lower / (scale_squared + residual**2)) ** count

    assert witness > Q(13, 10) * ceiling
    # exp(25) < 2^50. The log derivative on [1, 2] is strictly negative.
    assert 1 + Q(far**2, 2 * width**2) > 2**50
    assert Q(1, width**(2 * m - 2)) < ceiling / 4**n
    assert Q(36 * width**4, far**2) < ceiling / 4**n
    assert Q(2**n * far**4, (far - 1)**n) < ceiling / 4**n
    assert Q((2 * n)**n, far**(n - 4)) < ceiling / 4**n
    print(f"reflected example: exact witness/center lower bound {float(witness / ceiling):.12f}")


def scale_tie():
    p = 100
    n = 2 * p + 1
    slant = 2**20
    low = 2**10 * slant
    high = 2**100 * slant
    far = 2**50000
    width = 2**1000
    initial_scale = slant // 8
    late_scale = high // 16

    # Bernoulli bounds for the five factors other than the initial skew gain.
    assert 1 - Q(p, initial_scale**2) > Q(3, 4)
    assert 1 - p * Q(initial_scale, low)**2 > Q(3, 4)
    assert 1 - Q(p, slant**2) > Q(3, 4)
    assert 1 - Q(initial_scale, far)**2 > Q(3, 4)
    assert 1 - Q(1, slant**2) > Q(3, 4)
    assert Q(3, 4)**5 > Q(1, 8)
    assert Q(1, 64) + Q(1, slant**2) + 1 < Q(100, 81)
    early = Q(slant**2, 512) * Q(19, 10)**p
    separator = 64 * slant**2 * Q(9, 8)**p
    late_at_low = low**2 * Q(9, 8)**p
    early_upper = 64 * slant**2 * 2**p
    assert early > max(separator, late_at_low, 1)

    assert 1 - Q(p, late_scale**2) > Q(1, 2)
    assert 1 - Q(p, 16**2) > Q(1, 2)
    assert 1 - Q(p, slant**2) > Q(1, 2)
    assert 1 - Q(late_scale, far)**2 > Q(1, 2)
    assert 1 - Q(1, slant**2) > Q(1, 2)
    assert Q(late_scale**2, 32) > early_upper

    # Dropping the normalizing denominator 2^(p+1) only loosens both bounds.
    multiplier = 4**n * high**(2 * p)
    assert Q(multiplier * 2**n, width**(2 * p - 1)) < 1
    assert Q(multiplier * 12 * width**2, far) < 1
    assert 1 + Q(far**2, 2 * width**2) > 2**(20 * n)
    print("scale crossing: exact separator, end comparisons and df exclusions passed")


if __name__ == "__main__":
    reflected()
    scale_tie()
