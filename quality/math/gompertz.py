"""Gompertz distribution — symbolic spec.

Density (actuar parameterisation: shape=a, rate=b, both > 0, x > 0):
    log f = log(a) + b·x - (a/b)·(exp(b·x) - 1)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
a = sp.Symbol("a", positive=True)
b = sp.Symbol("b", positive=True)

log_density_expr = sp.log(a) + b * x_sym - (a / b) * (sp.exp(b * x_sym) - 1)

SPEC = Spec(
    id="gompertz",
    params=(
        Param(name="a", start=1.0, domain="(0, oo)"),
        Param(name="b", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    cdf=1 - sp.exp(-(a / b) * (sp.exp(b * x_sym) - 1)),
    references={"r": "extraDistr::dgompertz(x, a, b)"},
)
