"""Cauchy distribution — symbolic spec.

Density (in natural params (location, scale), scale > 0):
    log f = -log(π) - log(scale) - log(1 + ((x - location) / scale)²)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
location = sp.Symbol("location", real=True)
scale = sp.Symbol("scale", positive=True)

z = (x_sym - location) / scale
log_density_expr = -sp.log(sp.pi) - sp.log(scale) - sp.log(1 + z**2)

SPEC = Spec(
    id="cauchy",
    params=(
        Param(name="location", start=0.0, domain="real"),
        Param(name="scale", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    cdf=sp.Rational(1, 2) + sp.atan(z) / sp.pi,
    fisher_info_per_obs=sp.Matrix(
        [[1 / (2 * scale**2), 0], [0, 1 / (2 * scale**2)]]
    ),
    references={"r": "stats::dcauchy(x, location, scale)"},
)
