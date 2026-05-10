"""Inverse-Gaussian distribution — symbolic spec.

Density (in natural params (mean=μ, shape=λ), both > 0, x > 0):
    log f = -0.5·log(2π·x³) + 0.5·log(λ) - λ·(x - μ)² / (2·μ²·x)

Cost::operator() takes natural params directly (golden's `mean`/`shape`
keys match the spec param names).
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
mean_sym = sp.Symbol("mean", positive=True)
lam = sp.Symbol("shape", positive=True)
half = sp.Rational(1, 2)

log_density_expr = (
    -half * sp.log(2 * sp.pi * x_sym**3)
    + half * sp.log(lam)
    - lam * (x_sym - mean_sym) ** 2 / (2 * mean_sym**2 * x_sym)
)

SPEC = Spec(
    id="invgauss",
    params=(
        Param(name="mean", start=1.0, domain="(0, oo)"),
        Param(name="shape", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    references={"r": "statmod::dinvgauss(x, mean, shape)"},
)
