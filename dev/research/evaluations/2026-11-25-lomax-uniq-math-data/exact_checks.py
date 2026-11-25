"""Rational checks for the inequalities and polynomial signs in the report."""
from fractions import Fraction as Q

import sympy as sp


def log_bounds(x):
    """Rational lower and upper bounds for log of a positive rational."""
    x = Q(x)
    k = 0
    while x < 1:
        x *= 2
        k -= 1
    while x >= 2:
        x /= 2
        k += 1

    def series(y):
        t = (y - 1) / (y + 1)
        lower = 2 * sum(t ** (2*j + 1) / (2*j + 1) for j in range(24))
        upper = lower + 2*t**49 / (49*(1-t*t))
        return lower, upper

    lo, hi = series(x)
    l2, u2 = series(Q(2))
    return lo + k*(l2 if k >= 0 else u2), hi + k*(u2 if k >= 0 else l2)


def main():
    n = 21
    p = Q(20, n)
    mlo, mhi = Q(1010, n), Q(1011, n)
    ln2, un2 = log_bounds(2)
    ln10, un10 = log_bounds(10)
    # Uniform witness at rate 1/10 (using a <= 1).
    witness = (log_bounds(mlo/10)[0])
    a_upper = (11*log_bounds(Q(11, 10))[1] + 10*log_bounds(11)[1])/n
    witness -= log_bounds(a_upper)[1] + a_upper
    assert witness > Q(1, 10)
    # Right-hand upper envelope evaluated at the separator and at T.
    for multiplier in (10, 32):
        amin = p*(multiplier*ln2 + ln10)
        upper = multiplier*un2 + log_bounds(mhi)[1] - log_bounds(amin)[0] - amin
        assert upper < 0
    # Upper bound on all left-hand values.
    left_upper = log_bounds(mhi*1025/p)[1]
    assert left_upper < 11
    # High-rate witness at the lower a endpoint.
    a_upper = p*(512*un2 + un10) + un2
    high_witness = 512*ln2 + log_bounds(mlo)[0] - log_bounds(a_upper)[1] - a_upper
    assert high_witness > 11
    print("Rational comparisons passed:",
          tuple(float(x) for x in (witness, left_upper, high_witness)))

    rate, a = sp.symbols("r a")
    supports = [1, 100, a]
    factors = [1+rate*x for x in supports]
    denominator = sp.prod(factors)
    numerator = sum(
        sp.Rational(k, n)*sp.prod(v for j, v in enumerate(factors) if j != i)
        for i, k in enumerate((10, 10, 1))
    )
    poly = score_polynomial(rate, denominator, numerator)
    signs = []
    for j in range(1, poly.degree()+1):
        coefficient = sp.Poly(poly.nth(j), a)
        if coefficient.is_zero:
            signs.append(0)
            continue
        lowest = min(exponents[0] for exponents, c in coefficient.terms())
        leading = coefficient.nth(lowest)
        rest = sum(abs(c)*sp.Rational(1, 2)**(32*(exponents[0]-lowest))
                   for exponents, c in coefficient.terms() if exponents[0] > lowest)
        assert abs(leading) > rest
        signs.append(int(sp.sign(leading)))
    assert poly.nth(0) == 0
    nonzero = [sign for sign in signs if sign]
    assert sum(a != b for a, b in zip(nonzero, nonzero[1:])) <= 3
    print("Three-point polynomial signs:", signs)

    denominator = (1+rate)*(1+100*rate)
    numerator = (2+101*rate)/2
    coeffs = score_polynomial(rate, denominator, numerator).all_coeffs()
    print("Equal-weight (1,100) polynomial signs:",
          [int(sp.sign(c)) for c in reversed(coeffs)])

    # A smaller dyadic-free example located by the structured search.
    scores = []
    for rate, sign in zip((Q(1, 100000), Q(1, 1000), Q(1, 10), Q(1)),
                          (1, -1, 1, -1)):
        scaled = [rate*x for x in (1, 3000, 3000, 22000)]
        log_intervals = [log_bounds(1+x) for x in scaled]
        b = sum(x/(1+x) for x in scaled)/4
        lower, upper = [sum(bounds[j] for bounds in log_intervals)/4 - b/(1-b)
                        for j in range(2)]
        assert lower > 0 if sign > 0 else upper < 0
        scores.append((float(lower), float(upper)))
    print("Four-observation H at the four rates:", scores)

def score_polynomial(rate, denominator, numerator):
    return sp.Poly((denominator-numerator)*numerator**2
                   - rate*denominator*(numerator*sp.diff(denominator, rate)
                                       - sp.diff(numerator, rate)*denominator), rate)


if __name__ == "__main__":
    main()
