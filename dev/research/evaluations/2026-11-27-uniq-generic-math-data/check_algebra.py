"""Exact symbolic checks for the Student and skew-Student proofs."""

import sympy as sp


def check_student():
    y, d, a = sp.symbols("y d a", real=True)
    product = ((y - d)**2 + a**2) * ((y + d)**2 + a**2)
    assert sp.expand(product - (y**2 + a**2 - d**2)**2 - 4 * a**2 * d**2) == 0

    x, c, e, b = sp.symbols("x c e b")
    difference = (x - c) * ((x - e)**2 + b) - (x - e) * ((x - c)**2 + a)
    assert sp.Poly(difference, x).degree() == 2
    assert sp.expand(difference).coeff(x, 2) == c - e


def check_skew_student():
    r, gap, near_width, far_width, near_p, far_p = sp.symbols(
        "r gap near_width far_width near_p far_p"
    )
    polynomial = (
        far_p * (r**2 + near_width) * (r + gap)
        - near_p * ((r + gap)**2 + far_width) * r
    )
    coefficients = sp.Poly(polynomial, r).all_coeffs()
    assert coefficients[0] == far_p - near_p
    assert sp.expand(coefficients[1] - gap * (far_p - 2 * near_p)) == 0
    assert coefficients[-1] == far_p * near_width * gap

    u, fraction = sp.symbols("u fraction")
    right_far = u + 1
    right_mean = right_far - fraction
    denominator = fraction + (2 * fraction - 1) * u
    p_scaled = (2 * u + 1) * fraction * (1 - fraction) / denominator
    broad_square = u * right_far * (1 - fraction + (1 - 2 * fraction) * u) / denominator
    divisor = 4 * u**2 + (5 - 2 * fraction) * u + 1 - fraction
    profile = (2 * u + 1) * right_mean**2 / divisor
    assert sp.cancel(p_scaled / (1 + broad_square / right_mean**2) - profile) == 0
    assert sp.cancel(p_scaled / fraction - u - broad_square / u) == 0
    assert sp.cancel(p_scaled / (1 - fraction) - right_far - broad_square / right_far) == 0
    derivative = right_mean * (8 * u**3 + 12 * u**2 + 3 * u - 1 + fraction) / divisor**2
    assert sp.cancel(sp.diff(profile, u) - derivative) == 0
    normal_ratio = u / (2 * u + 1)
    assert sp.cancel(
        (profile - right_mean**2 / (2 * u + 1)).subs(fraction, normal_ratio)
    ) == 0


if __name__ == "__main__":
    check_student()
    check_skew_student()
    print("Exact algebra checks passed for std and sstd.")
