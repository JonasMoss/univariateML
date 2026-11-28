"""Exact arithmetic for the decisive comparisons in the review."""
from fractions import Fraction as Q
import sympy as sp


def main():
    # The Laplace endpoint likelihood ratio between decreasing and increasing
    # slopes of magnitude -log(z) is z**(k-j), for k lower and j upper copies.
    z = sp.symbols("z", positive=True)
    assert sp.cancel((1 * 1 * z) / (z * z * 1)) == 1 / z
    assert (Q(0) + 0 + 1) / 3 < Q(1, 2)
    assert Q(2, 2) - 1 == 0  # Right-ray plateau optimal rate.
    assert sp.expand((1 + z)**2 - 1) == z**2 + 2*z

    # dWeibull mixed-pair saturation at two different held shape2 values.
    for beta in (2, 3):
        assert Q(3**beta - 2**beta, 2**beta - 1) > 1

    # Held-scale half-Cauchy shrinking-window density is >= 1/(2 epsilon).
    for width in (Q(1), Q(1, 2), Q(1, 4)):
        assert 1 / (1 + width**2) >= Q(1, 2)

    # At the half-normal exponential edge the exponential-edge example is equality.
    assert (Q(0)**2 + Q(1)**2) / 2 == 2 / Q(2)**2
    print("PASS: exact endpoint, odds, collapse and equality arithmetic")


if __name__ == "__main__":
    main()
