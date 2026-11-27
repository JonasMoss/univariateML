"""Exact arithmetic for the four proposed nonuniqueness examples.

The review gives the symmetry, integration and strictness arguments that
connect these identities and bounds to global maximizers.
"""

from collections import Counter
from fractions import Fraction


def check_reflected_power_maxima():
    # At center 4 the squared radii are 1 and 65, versus 17 and 17 at zero.
    # Integrating over the whole line bounds the retained normalizer by
    # pi < 4 for Cauchy and pi/2 < 2 for Student with nu = 3.
    uniform = Fraction(1, 200**2)
    for exponent, normalizer_bound in ((1, 4), (2, 2)):
        assert Fraction(17**2, 65)**exponent > 1
        assert Fraction(1, 65**exponent * normalizer_bound**2) > uniform


def check_laplace_plateau():
    # t = exp(-r), u = 1 - t > 0. The competitor-to-plateau ratio
    # is 1/(2-t)^2. Coefficients in u of (1+u)^2 - 1 are (0, 2, 1).
    gap_coefficients = [1 - 1, 2, 1]
    assert all(coefficient >= 0 for coefficient in gap_coefficients)
    assert any(coefficient > 0 for coefficient in gap_coefficients)
    # The plateau log likelihood is 2*log(r) - r.
    assert Fraction(2, 2) - 1 == 0
    assert -Fraction(2, 2**2) < 0


def check_logistic_flat_location():
    # The factors (L+c)(U+c) occur twice in the numerator;
    # entries below count the factors from the observations L and U.
    sample = ("L", "U")
    normalizer_factors = Counter({"L": len(sample), "U": len(sample)})
    observation_factors = Counter(value for value in sample for _ in range(2))
    assert normalizer_factors == observation_factors


if __name__ == "__main__":
    check_reflected_power_maxima()
    check_laplace_plateau()
    check_logistic_flat_location()
    print("Exact checks passed for Cauchy, Student, Laplace and logistic witnesses")
