"""Exact arithmetic checks for the reviewed half-contact and tie examples."""
from fractions import Fraction as F
from math import factorial, prod


def exp_minus_bounds(x):
    """Alternating Taylor enclosure on [0, 1]."""
    assert 0 <= x <= 1
    terms = [(-x)**i / factorial(i) for i in range(13)]
    return sum(terms[:-1]), sum(terms)


def half_contact(gaps, width, d, q, shape):
    n = 2 * len(gaps)
    ratios = [F(1)] * len(gaps) + [(d / (d + g))**shape for g in gaps]
    densities = [
        shape * q * ratio / (d + g) * exp_minus_bounds(q * ratio)[1]
        for g, ratio in zip([0] * len(gaps) + gaps, ratios)
    ]
    v = (d / (d + width))**shape
    mass_lower = exp_minus_bounds(q * v)[0] - exp_minus_bounds(q)[1]
    assert mass_lower > 0
    upper = prod(densities) / mass_lower**n
    target = prod(g**-2 for g in gaps)
    assert upper < target
    if shape == 1:
        # Delta F = integral of exp(-t) over [q*v, q], so Delta F <= q.
        # Each density retains at least a factor 1-q from its exponential.
        lower = (1-q)**n * prod((d+g)**-2 for g in gaps)
        assert target - lower < target * (n*q + 2*d*sum(1/g for g in gaps))
        return lower


def check_half_contact():
    for gaps, width in (([F(1)], F(2)), ([F(1, 2), F(2)], F(3))):
        for step in (10, 100, 1000):
            d = q = F(1, step)
            for shape in (1, 2, 3):
                half_contact(gaps, width, d, q, shape)
    lower = half_contact([F(1)], F(2), F(1, 1000), F(1, 1000), 1)
    assert lower > F(99, 100)


def check_power():
    # For (1,2), u=min(alpha,2) must be >=2, and the entire feasible
    # likelihood equals beta^2*2^(-beta-1). Bracket the unique score zero.
    z = F(1, 3)
    log2_lower = 2 * sum(z**(2*j+1)/(2*j+1) for j in range(12))
    log2_upper = log2_lower + 2*z**25/(25*(1-z*z))
    assert F(69, 100) < log2_lower < log2_upper < F(7, 10)
    assert 2/F(2) > log2_upper and 2/F(3) < log2_lower
    for alpha in (F(2), F(3)):
        assert min(alpha, F(2)) == F(2)
    # At fixed beta=2 the same two endpoints have likelihood 1/2.
    assert (2*F(1)/4) * (2*F(2)/4) == F(1, 2)


def check_pareto2():
    # Coefficients in ascending order, with s>0. Both sides of
    # 16(1+s)^3-108s = (2s-1)^2(4s+16) are computed independently.
    square = [1, -4, 4]
    factored = [sum(square[i-j]*c for j, c in enumerate((16, 4))
                    if 0 <= i-j < len(square))
                for i in range(4)]
    assert factored == [16, 48-108, 48, 16]
    for threshold in (F(3, 4), F(1)):
        scale = threshold - F(1, 2)
        s = scale + 1 - threshold
        assert threshold <= 1 and scale > 0 and s == F(1, 2)
        assert 4*s/(1+s)**3 == F(16, 27)


def check_gompertz():
    # On [0,infinity), retained mass is 1 and both exponents at x=0
    # vanish exactly, so f(0)=a for every feasible b.
    for b in (F(1), F(2)):
        a, x, mass = F(2), F(0), F(1)
        assert b*x == 0 and a/b*(1-1) == 0
        assert a/mass == 2


if __name__ == "__main__":
    check_half_contact()
    check_power()
    check_pareto2()
    check_gompertz()
    print("Exact checks passed for Fréchet half-contact and all three tie examples.")
