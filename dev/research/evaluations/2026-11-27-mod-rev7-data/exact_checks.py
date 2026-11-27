"""Exact arithmetic for the strict inequalities in the three tie witnesses."""
from fractions import Fraction as Q


def main():
    # Fixed-location shift from 0 to 4 on the symmetric window lowers the
    # normalizer for both strictly decreasing radial kernels.
    uniform = Q(1, 200**2)
    # LST with df 3 and effective scale 1. Z <= pi/2 < 2.
    assert Q(17**2, 65)**2 > 1
    assert Q(1, 65**2 * 2**2) > uniform
    # Slash at scale 1. At 4 the product is (1-exp(-32))/128.
    # exp(32) >= 33 > 2, while the centered product is < 1/16**2.
    assert 1 + 32 > 2
    assert Q(1, 128) / 2 == Q(1, 16**2)
    # Split the whole-line normalizer at |u|=1.
    assert 2 * Q(1, 2) + 2 == 3
    assert Q(1, 256 * 3**2) > uniform

    # Double Pareto at rates 1. The log of k(y)/k(0) minus -y
    # is c-abs(y-c)+y. Expand it on y>=c and 0<=y<=c.
    # Coefficients are ordered (c,y).
    for sign, expected in ((1, (2, 0)), (-1, (0, 2))):
        assert (1 + sign, 1 - sign) == expected
    # For c>0, an exact point in (0,min(c,1)) gives positive gap,
    # and continuity gives a positive-integral difference.
    for c in (Q(1, 4), Q(1), Q(2)):
        y = min(c, 1) / 2
        assert 0 < y < min(c, 1) and c - abs(y - c) + y == 2*y
    print("Exact reflected LST and Slash bounds and Double Pareto plateau checks passed.")


if __name__ == "__main__":
    main()
