"""Logistic distribution — symbolic spec.

Density (in natural params (location, scale), scale > 0):
    log f = -z - log(scale) - 2 log(1 + exp(-z)),    z = (x - location) / scale
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
location = sp.Symbol("location", real=True)
scale = sp.Symbol("scale", positive=True)

z = (x_sym - location) / scale
log_density_expr = -z - sp.log(scale) - 2 * sp.log(1 + sp.exp(-z))

SPEC = Spec(
    id="logis",
    params=(
        Param(name="location", start=0.0, domain="real"),
        Param(name="scale", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    cdf=1 / (1 + sp.exp(-z)),
    references={"r": "stats::dlogis(x, location, scale)"},
)
