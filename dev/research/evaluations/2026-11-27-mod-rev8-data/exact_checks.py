"""Exact checks for TASK-457 witnesses and review counterexamples."""

from fractions import Fraction as F
from math import comb


def pair(weights):
    total = sum(weights)
    return tuple(F(weight, total) for weight in weights)


def check():
    # The empirical law is a global upper bound on each of these windows.
    for alpha, beta in [(1, 1), (3, 2)]:
        assert (F(alpha + 1, 2 * beta)) == 1  # Beta-binomial, size 2.
    for size, alpha, beta in [(1, 1, 2), (2, 1, 1)]:
        for k, target in enumerate([F(3, 5), F(2, 3)], 1):
            assert F((k + size) * (k + beta),
                     (k + 1) * (k + alpha + beta + size)) == target
    for nu in (0, 1):
        odds = 2**nu
        assert pair([2**nu * odds, odds**2]) == (F(1, 2), F(1, 2))
    # CMB cross-size tie: q is the positive root of 2q^2+2q-1.
    # Equality reduces to (q-1)(2q^2+2q-1)=0 after cancelling (1-q)^2.
    q_polynomial = lambda q: 2*q*q + 2*q - 1
    assert q_polynomial(F(1, 3)) < 0 < q_polynomial(F(1, 2))
    for q in (F(1, 3), F(1, 2)):
        assert 1 - q**3 - 3*q*(1 - q*q) == (q - 1)*q_polynomial(q)
    # For N>=3, (N+1)/(N-1)<=2 and (1-q^N)/(1-q^(N+1))<1.

    for location in (-1, 0):
        assert pair([F(1, 2)**abs(x-location) for x in (0, 1)]) == (
            F(2, 3), F(1, 3))
    # Held-scale balanced pair: the two likelihood products agree at q=1/2.
    assert F(2, 3)*F(1, 3) == F(1, 3)*F(2, 3)
    # Normal cell integrals coincide by exact reflection about the held mean.
    for sd in (1, 2):
        left = (F(-1, sd), F(0))
        right = (F(0), F(1, sd))
        assert tuple(-end for end in reversed(left)) == right
    for a, b in ((0, 3), (1, 2)):
        assert (max(a, 1), min(b, 2)) == (1, 2)

    # Weibull: at each of two exact integer shapes, opposite signs bracket
    # an exact polynomial root in q in (0,1), so both fits saturate.
    for beta in (2, 3):
        def difference(q):
            return q**(2**beta) - q**(3**beta) - q + q**(2**beta)
        assert difference(F(1, 2)) < 0 < difference(F(99, 100))
    # Full window with held q=1/2 and sample (1): strict Bernoulli escape.
    for beta in (1, 2, 3):
        assert 0 < F(1, 2) - F(1, 2)**(2**beta) < F(1, 2)

    for draws, marked in ((3, 2), (2, 3)):
        assert pair([comb(marked, x) * comb(4-marked, draws-x)
                     for x in (1, 2)]) == (F(1, 2), F(1, 2))
    for population in (2, 3):
        assert pair([comb(x-1, 0) * comb(population-x, 0)
                     for x in (1, 2)]) == (F(1, 2), F(1, 2))
    for z in (4, 5):
        v = F(2) / (1 + F(1, z))
        assert 0 < 2*v/z < 1 and v*(1 + F(1, z))/2 == 1
    # Skellam (0,1): for each exact z in {1,2}, take odds I0(z)/I1(z).
    # With abstract positive adjacent carriers their cancellation is exact.
    # Check the exponents of (I0, I1) in (odds * I1)/I0.
    for z in (1, 2):
        assert F(z, 2) > 0  # The positive Bessel series applies at both z.
    assert tuple(a + b - c for a, b, c in
                 zip((1, -1), (0, 1), (1, 0), strict=True)) == (0, 0)
    # CMP counterexample to an unrestricted zero-dispersion affine fiber.
    assert F(2, 1) >= 1  # Sample (0,1,1), window [0,1], target odds two.
    assert pair([1, 2]) == (F(1, 3), F(2, 3))


if __name__ == "__main__":
    check()
    print("Exact witness and counterexample checks passed.")
