"""Kumaraswamy distribution — symbolic spec.

Density (in natural params (a, b), a > 0, b > 0, x in (0, 1)):
    log f = log(a) + log(b) + (a-1)*log(x) + (b-1)*log(1 - x^a)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
a = sp.Symbol("a", positive=True)
b = sp.Symbol("b", positive=True)

log_density_expr = (
    sp.log(a)
    + sp.log(b)
    + (a - 1) * sp.log(x_sym)
    + (b - 1) * sp.log(1 - x_sym**a)
)

SPEC = Spec(
    id="kumar",
    params=(
        Param(name="a", start=1.0, domain="(0, oo)"),
        Param(name="b", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, 1)",
    cdf=1 - (1 - x_sym**a) ** b,
    references={"r": "extraDistr::dkumar(x, a, b)"},
)
