"""Gumbel distribution — symbolic spec.

Density (in natural params (mu, sigma), sigma > 0):
    log f = -log(sigma) - z - exp(-z),    z = (x - mu) / sigma
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
mu = sp.Symbol("mu", real=True)
sigma = sp.Symbol("sigma", positive=True)

z = (x_sym - mu) / sigma
log_density_expr = -sp.log(sigma) - z - sp.exp(-z)

SPEC = Spec(
    id="gumbel",
    params=(
        Param(name="mu", start=0.0, domain="real"),
        Param(name="sigma", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    cdf=sp.exp(-sp.exp(-z)),
    references={"r": "extraDistr::dgumbel(x, mu, sigma)"},
)
