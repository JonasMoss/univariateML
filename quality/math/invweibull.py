"""Inverse Weibull distribution — symbolic spec.

Density (in natural params (shape=s, rate=r), actuar parameterisation
where rate = 1/scale, both > 0, x > 0):
    log f = log(s) - s·log(r) - (s+1)·log(x) - (r·x)^(-s)

(Per the C++ comment in invweibull/mle.hpp; the - s*log(r) form follows
from rate = 1/scale and scale^s appearing inside the exp.)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
s = sp.Symbol("shape", positive=True)
r = sp.Symbol("rate", positive=True)

rx = r * x_sym
log_density_expr = (
    sp.log(s) - s * sp.log(r) - (s + 1) * sp.log(x_sym) - rx ** (-s)
)

SPEC = Spec(
    id="invweibull",
    params=(
        Param(name="shape", start=2.0, domain="(0, oo)"),
        Param(name="rate", start=2.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    references={"r": "actuar::dinvweibull(x, shape, rate)"},
)
