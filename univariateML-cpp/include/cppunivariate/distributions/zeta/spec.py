"""Differentiable Euler--Maclaurin oracle for the exact README zeta density.

The quality harness cannot lambdify derivatives of SymPy's zeta. Use 64
explicit terms and four endpoint corrections instead; this independently
checks the fitter's 1024-term moment calculation and Boost normalizer.
This is a numerical oracle approximation, not a new probability law.
"""
import sympy as sp


def log_density(symbols, x):
    s = symbols['s']
    a = sp.Integer(64)
    normalizer = sum(sp.Integer(k)**(-s) for k in range(1, 65))
    normalizer += a**(1-s)/(s-1) - a**(-s)/2
    for order, coefficient in ((1, sp.Rational(1, 12)),
                               (3, -sp.Rational(1, 720)),
                               (5, sp.Rational(1, 30240)),
                               (7, -sp.Rational(1, 1209600))):
        normalizer += coefficient*sp.rf(s, order)*a**(-s-order)
    return -s*sp.log(x)-sp.log(normalizer)
