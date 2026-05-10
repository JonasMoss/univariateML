"""Inverse-Gamma distribution — symbolic spec.

Density (in natural params (alpha, beta), both > 0, x > 0):
    log f = α·log(β) - lgamma(α) - (α+1)·log(x) - β/x

Closed-form Fisher info per obs (data-independent):
    I_aa = trigamma(α)
    I_bb = α / β²
    I_ab = -1 / β
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
a = sp.Symbol("alpha", positive=True)
b = sp.Symbol("beta", positive=True)

log_density_expr = (
    a * sp.log(b) - sp.loggamma(a) - (a + 1) * sp.log(x_sym) - b / x_sym
)

SPEC = Spec(
    id="invgamma",
    params=(
        Param(name="alpha", start=2.0, domain="(0, oo)"),
        Param(name="beta", start=2.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    fisher_info_per_obs=sp.Matrix(
        [[sp.polygamma(1, a), -1 / b], [-1 / b, a / b**2]]
    ),
    references={"r": "extraDistr::dinvgamma(x, alpha, beta)"},
)
