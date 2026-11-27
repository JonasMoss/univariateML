"""Exact arithmetic for the skew-t crossing and symbolic score checks."""
from fractions import Fraction

import sympy as sp


def check_crossing():
    p, A, R = 100, 2**20, 2**50000
    lo, hi, W = 2**10 * A, 2**150 * A, 2**1000
    n = 2 * p + 1
    E = Fraction(A, 512) * Fraction(19, 10)**p

    w = Fraction(A, 8)
    # Lower-bound the five factors other than S_1**p at the small witness.
    assert 1 + A*A + w*w < Fraction(100, 81)*A*A
    assert 1 - p/(1 + w*w) > Fraction(3, 4)
    assert 1 - p*w*w/(lo*lo + w*w) > Fraction(3, 4)
    assert 1 - Fraction(p, A*A) > Fraction(3, 4)
    assert 1 - w*w/(R*R + w*w) > Fraction(3, 4)
    assert 1 - Fraction(1, A*A) > Fraction(3, 4)
    assert Fraction(3, 4)**5 > Fraction(1, 64)

    assert E > 1
    assert 8*A*Fraction(9, 8)**p < E
    assert lo*Fraction(9, 8)**p < E
    w = Fraction(hi, 16)
    assert w < R
    assert 1 - p/(1 + w*w) > Fraction(1, 2)
    assert 1 - p*w*w/(hi*hi + w*w) > Fraction(1, 2)
    assert w/32 > 8*A*2**p

    # Factor pi**n / 2**(p+1) is bounded by 4**n.
    norm_upper = 4**n * hi**(2*p)
    assert norm_upper * 2**n < W**(2*p-1)
    assert norm_upper * 12*W**2 < R
    assert R**2 > 2*W**2 * 2**(20*n)

    # Exact coefficient in the normalization with one remote observation.
    v, D, F = sp.symbols("w D R", positive=True)
    old = (v/(v*v+1))**p * (v/(v*v+D*D))**p * v/(v*v+F*F)
    new = v*(v*v/(v*v+1))**p*(D*D/(v*v+D*D))**p*F*F/(v*v+F*F)
    assert sp.cancel(old*D**(2*p)*F*F/new) == 1
    print("Skew-t normalization, witness and separating bounds: pass (exact).")


def check_scores():
    x, m, a, b = sp.symbols("x m a b", real=True)
    fixed = sp.expand((x-m)*((x-b)**2+a*a) - (x-b)*((x-m)**2+a*a))
    assert sp.Poly(fixed, x).coeff_monomial(x*x) == m-b
    midpoint = (m*m+a*a-b*b)**2 + 4*a*a*b*b
    assert sp.expand(((m-b)**2+a*a)*((m+b)**2+a*a) - midpoint) == 0

    # Huber log-height along the one-core affine path at fixed tail slope.
    t, j, g = sp.symbols("t j g", real=True)
    shift = b*b*t*g
    core_log = -(x-(x+shift))**2/(2*b*b*t)
    tail_log = j*t/2 - j*g*shift
    assert sp.diff(j*core_log + tail_log, t, 2).simplify() == 0

    k = sp.symbols("k", positive=True)
    assert sp.simplify(sp.diff(k*sp.tanh(k*x)/x, x) -
                       k*(k*x*sp.sech(k*x)**2-sp.tanh(k*x))/x**2) == 0
    i, j = sp.symbols("i j", nonnegative=True, integer=True)
    ratio_difference = (2*i+1)/(2*i+3) - (2*j+1)/(2*j+3)
    assert sp.cancel(ratio_difference - 4*(i-j)/((2*i+3)*(2*j+3))) == 0
    print("Student, Huber and folded-normal score identities: pass (symbolic).")


if __name__ == "__main__":
    check_crossing()
    check_scores()
