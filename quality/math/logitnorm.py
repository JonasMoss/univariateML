"""Logit-Normal distribution — symbolic spec.

Density (in natural params (mu, sigma), sigma > 0, x in (0, 1)):
    log f = -log(x·(1-x)) - 0.5·log(2π) - log(sigma)
          - (logit(x) - mu)² / (2·sigma²)

The C++ MLE fits via Normal MLE on logit(x); the change-of-variable
Jacobian -log(x·(1-x)) is data-only and drops out of every parameter
derivative.
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
mu = sp.Symbol("mu", real=True)
sigma = sp.Symbol("sigma", positive=True)
half = sp.Rational(1, 2)

logit_x = sp.log(x_sym / (1 - x_sym))

log_density_expr = (
    -sp.log(x_sym * (1 - x_sym))
    - half * sp.log(2 * sp.pi)
    - sp.log(sigma)
    - (logit_x - mu) ** 2 / (2 * sigma**2)
)

SPEC = Spec(
    id="logitnorm",
    params=(
        Param(name="mu", start=0.0, domain="real"),
        Param(name="sigma", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, 1)",
    references={"r": "logitnorm::dlogitnorm(x, mu, sigma)"},
)
