"""Interval certificates for exact real GED examples."""
from fractions import Fraction
from functools import lru_cache
from mpmath import iv

iv.dps = 40


def interval(q):
    q = Fraction(q)
    return iv.mpf(q.numerator) / q.denominator


def loggamma(x):
    # Shift to y >= 16. The positive-axis Stirling remainder has
    # the sign of and magnitude at most the first omitted term.
    y = x + 16
    correction = sum(iv.ln(x + j) for j in range(16))
    base = (y - interval('0.5')) * iv.ln(y) - y + iv.ln(2 * iv.pi) / 2
    terms = 1/(12*y) - 1/(360*y**3) + 1/(1260*y**5) - 1/(1680*y**7)
    error = 1/(1188*y**9)
    return base + terms - correction + iv.mpf([0, 1]) * error


@lru_cache(maxsize=None)
def components(p):
    t = 1 / interval(p)
    g1, g3 = loggamma(1 + t), loggamma(1 + 3*t)
    peak = (g3 - 3*g1)/2
    multiplier = iv.exp((g3 - g1)/(2*t))
    return peak, multiplier


def penalty(p, radius):
    r = iv.sqrt(3)
    _, multiplier = components(p)
    outer = interval(Fraction(float("1.4239025460520294")))
    return multiplier * (46 * (interval(radius)/r)**interval(p)
                         + 24 * (outer/r)**interval(p)) / 78


def value(p, radius):
    return components(p)[0] - penalty(p, radius)


def cover(left, right, radius, witness):
    pending = [(Fraction(left), Fraction(right))]
    cells = 0
    while pending:
        a, b = pending.pop()
        cells += 1
        assert cells < 30000, (a, b, radius)
        upper = components(a)[0] - penalty(b, radius)
        if upper < witness:
            continue
        assert b-a > Fraction(1, 2**30), (float(a), float(b), radius, upper, witness)
        mid = (a + b) / 2
        pending.extend([(a, mid), (mid, b)])
    return cells


def balanced_contact(gamma_lower, gamma_upper):
    limit_penalty = iv.exp(-gamma_lower)
    cutoff = interval(Fraction(1, 100))
    assert (interval(Fraction(9, 4))*cutoff <
            2*iv.exp(-gamma_upper)/(1+3*cutoff)**2)
    pending = [(Fraction(1), Fraction(100))]
    cells = 0
    while pending:
        a, b = pending.pop()
        cells += 1
        assert cells < 10000
        if components(a)[0] - components(b)[1] + limit_penalty < 0:
            continue
        assert b-a > Fraction(1, 2**30), (float(a), float(b))
        mid = (a+b)/2
        pending.extend([(a, mid), (mid, b)])
    return cells


def main():
    print("40-digit interval cover")
    low, high = '0.329', '0.331'
    low_shape, high_shape = Fraction('1.11'), Fraction('25.4')
    witness = value(high_shape, high)
    assert components(Fraction(100))[0] < witness
    assert value(Fraction(1), low) < value(low_shape, low)
    assert value(Fraction(1), low) < witness
    # Both candidates are above the uniform face (relative value zero).
    assert witness > 0
    valley = cover(2, 8, low, witness)
    print("radius-band valley, cells:", valley)
    left_wins = cover(8, 100, low, value(low_shape, low))
    print("small-radius right exclusion, cells:", left_wins)
    right_wins = cover(1, 2, high, value(high_shape, high))
    print("large-radius left exclusion, cells:", right_wins)
    binary = Fraction(float("0.33"))
    binary_left = value(low_shape, binary)
    binary_right = value(high_shape, binary)
    assert value(Fraction(1), binary) < binary_left
    binary_valley = cover(2, 8, binary, min(binary_left, binary_right))
    print("binary64 separated interior peaks, cells:", binary_valley)

    # psi(2) = 1-gamma. The elementary harmonic/log enclosure for gamma
    # suffices for the strict derivative sign in the free-scale example.
    n = 128
    gamma_lower = sum(interval(Fraction(1, k)) for k in range(1, n+1))
    gamma_lower -= iv.ln(n) + interval(Fraction(1, 2*n))
    gamma_upper = gamma_lower + interval(Fraction(1, 12*n*n))
    assert 3 * iv.exp(gamma_upper - 1) < 2
    a = 3 / iv.exp(1) - 1
    assert interval('0.103') < a < interval('0.105')
    a = interval('0.105')
    entropy_deficit = iv.ln(3/(1+a)) + a * iv.ln(a)/(1+a)
    assert 2 - gamma_lower - iv.ln(2) < entropy_deficit
    assert iv.ln(2) > 1/interval(2)
    print('free-scale Laplace/uniform crossing inequalities passed')
    print("exact balanced held-sd contact cover, cells:",
          balanced_contact(gamma_lower, gamma_upper))
    assert iv.ln(6)/2 > (iv.sqrt(6)-iv.exp(-gamma_upper))/4
    a = interval(Fraction(1, 1024))
    deficit = iv.ln(4/(1+3*a)) + 3*a*iv.ln(a)/(1+3*a)
    assert 4*iv.exp(gamma_upper-1) < 3
    assert 2-gamma_lower-2*iv.ln(2)+iv.ln(3) < deficit
    assert 1 + 3*a < 4/iv.exp(1)
    rounded_endpoint = interval(Fraction(float.fromhex("0x1.bb67ae8584caap+0")))
    delta = -iv.ln(rounded_endpoint/iv.sqrt(3))
    assert 0 < delta < interval("1e-15")
    e0 = iv.exp(-gamma_upper)
    assert iv.ln(6)/2 < interval("1.5")*iv.exp(-2*delta)
    assert iv.ln(6/iv.pi)/2 < e0*iv.exp(-4*delta)
    # Use zeta(2) = pi^2/6 in the bound on C(t).
    c = interval(Fraction(3, 8))*iv.pi**2/e0
    assert 16*iv.exp(-4*delta) > c
    assert 70**2*iv.exp(-70) > c*delta**2
    print("rounded endpoint positive-witness lower bound on shape passed")
    print("held-sd finite contact witness and all-free median interval passed")


if __name__ == '__main__':
    main()
