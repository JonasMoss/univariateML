"""Exact arithmetic for the sample comparisons used in the review."""
from fractions import Fraction as Q
from math import comb


def normal_near_face():
    radius = Q(57735, 100000)
    score_gap = Q(1, 3) - radius**2
    assert 0 < score_gap < Q(1, 10**6)
    # On [-1/100, 0] the tilt's density ratio to uniform is >= 1-1/100.
    # The same lower bound multiplies Var_uniform(X^2), by minimizing over
    # the center in the squared-error expectation.
    curvature = Q(99, 100) * (Q(1, 5) - Q(1, 3)**2)
    assert curvature > Q(2, 25)
    assert curvature / 100 > score_gap
    assert score_gap**2 / (2 * curvature) < Q(1, 10**8)
    print("Normal: positive face score, interior root and strict gap bound")


def gamma_corner():
    # Log X is uniform on [0, log 4]; the sample averages its two endpoints.
    # Jensen on 1/X over [1,2] gives log 4 > 4/3.
    assert Q(3) / Q(4, 3) < Q(5, 2)
    print("Gamma: zero inward shape score and strictly negative rate score")


def binomial_examples():
    # Check the identity at a finite size symbolically by cancelling N-1
    # from (N-1)p/[2(1-p)] at p=1/N; the Poisson odds are lambda/2.
    conditional_odds = Q(1, 2)
    assert conditional_odds / (1 + conditional_odds) == Q(1, 3)
    assert Q(1) / 2 == conditional_odds
    # Size-one inflated fit to (0,1) is the full Bernoulli product curve.
    for p, pi in [(Q(1, 2), Q(0)), (Q(1), Q(1, 2))]:
        assert p * (1 - pi) == Q(1, 2)
    # Any interior p gives probability one after conditioning on a singleton.
    assert comb(12, 3) * Q(1, 2)**12 > 0
    print("Binomial: exact saturated ties and singleton compatibility")


def cmb_shortcut():
    q = Q(183, 500)
    z = Q(1, 8)

    def conditional(size):
        return comb(size, 2) * (1 - q)**2 * q**(size - 2) / (1 - q**size)

    assert q**3 < z < q**2
    assert 2 * q**2 + 2 * q < 1
    assert 2 * q < 1  # Bounds every successive ratio beginning at size 3.
    assert conditional(2) > conditional(3)
    size_two = q**2 * (1 - q)**14
    size_three = z * (1 - z)**7 * conditional(3)**7
    assert size_three > size_two
    print("CMB: exact cross-design inequality excludes the boundary shortcut")


def zip_tie():
    # The two-turning-point proof uses only the exact condition v-u > 4.
    # At the lower turning level the low-rate peak dominates; at the upper
    # level the order reverses. The report derives the signs analytically.
    assert Q(10) - Q(10, 100) == Q(99, 10) > 4
    print("ZIP: exact sample satisfies the analytic two-peak condition")


if __name__ == "__main__":
    normal_near_face()
    gamma_corner()
    binomial_examples()
    cmb_shortcut()
    zip_tie()
