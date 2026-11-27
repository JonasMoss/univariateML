"""Check polynomial and derivative identities used in TASK-414."""

from fractions import Fraction

import sympy as sp


def main():
    r, d, a, b, u, v = sp.symbols("r d a b u v", positive=True)
    student = v * (r**2 + a) * (r + d) - u * ((r + d)**2 + b) * r
    assert sp.Poly(student, r).all_coeffs() == [
        -u + v, -2*d*u + d*v, a*v - b*u - d**2*u, a*d*v
    ]
    log_ratio = u * sp.log(r) - v * sp.log(r + d)
    assert sp.factor(sp.diff(log_ratio, r) * r * (r + d)) == d*u + r*u - r*v

    delta, q = sp.symbols("delta q", nonnegative=True)
    # Differentiate r(r-delta)/(1+r^2) on r >= delta.
    derivative = sp.factor(sp.diff(r * (r-delta) / (1+r**2), r))
    assert sp.factor(derivative - (delta*r**2 + 2*r - delta)/(1+r**2)**2) == 0
    assert sp.expand((delta*r**2 + 2*r - delta).subs(r, q+delta)).coeff(q, 0) == delta**3 + delta

    c, h, squared_width = sp.symbols("c h squared_width", real=True)
    product = ((h-c)**2+squared_width) * ((h+c)**2+squared_width)
    assert sp.expand(product - (c**2+squared_width-h**2)**2 - 4*h**2*squared_width) == 0

    z, k, variance, offset = sp.symbols("z k variance offset", real=True)
    w = sp.sqrt(variance + offset*z**2)
    assert sp.simplify(sp.diff(-k*z/w, z) + k*variance/w**3) == 0

    # Strict symmetric two-point witnesses (Student and GED).
    assert 100**6 > 9 * (1 + 4*100**2)**2
    assert 1 < Fraction(11, 10)**2 < Fraction(25, 18)
    print("Student intersections and scale signs, two-point reductions, SGED join derivative and rational witness checks passed.")


if __name__ == "__main__":
    main()
