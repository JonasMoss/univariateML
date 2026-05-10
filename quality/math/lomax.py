"""Lomax distribution — symbolic spec.

Density (in natural params (lambda, kappa), both > 0, x > 0):
    f(x; λ, κ) = λ · κ · (1 + λ·x)^(−(κ + 1))
    log f = log(λ) + log(κ) − (κ + 1)·log(1 + λ·x)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
lam = sp.Symbol("lambda", positive=True)
kap = sp.Symbol("kappa", positive=True)

log_density_expr = (
    sp.log(lam) + sp.log(kap) - (kap + 1) * sp.log(1 + lam * x_sym)
)

SPEC = Spec(
    id="lomax",
    params=(
        Param(name="lambda", start=1.0, domain="(0, oo)"),
        Param(name="kappa", start=2.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    cdf=1 - (1 + lam * x_sym) ** (-kap),
    references={"r": "extraDistr::dlomax(x, lambda, 1/kappa)"},
    notes="Note: extraDistr's lomax uses 'sigma = 1/kappa' for the second arg.",
)
