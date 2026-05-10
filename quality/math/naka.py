"""Nakagami distribution — symbolic spec.

Density (in natural params (shape=m, scale=Omega), both > 0, x > 0):
    log f = log(2) + m*log(m) - lgamma(m) - m*log(Ω) + (2m-1)*log(x) - m·x²/Ω
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
m = sp.Symbol("shape", positive=True)
W = sp.Symbol("scale", positive=True)

log_density_expr = (
    sp.log(2)
    + m * sp.log(m)
    - sp.loggamma(m)
    - m * sp.log(W)
    + (2 * m - 1) * sp.log(x_sym)
    - m * x_sym**2 / W
)

SPEC = Spec(
    id="naka",
    params=(
        Param(name="shape", start=2.0, domain="(0, oo)"),
        Param(name="scale", start=2.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    references={"r": "extraDistr::dnaka(x, shape, scale)"},
)
