"""Rayleigh distribution — symbolic spec.

Density (in natural param sigma > 0, x > 0):
    f(x) = (x / sigma²) · exp(-x² / (2 sigma²))

Closed-form MLE: sigma = sqrt(mean(x²) / 2).
Fisher info per obs: I(sigma) = 4 / sigma².
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
sigma = sp.Symbol("sigma", positive=True)

log_density_expr = (
    sp.log(x_sym) - 2 * sp.log(sigma) - x_sym**2 / (2 * sigma**2)
)

SPEC = Spec(
    id="rayleigh",
    params=(Param(name="sigma", start=1.0, domain="(0, oo)"),),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    fisher_info_per_obs=sp.Matrix([[4 / sigma**2]]),
    references={"r": "extraDistr::drayleigh(x, sigma)"},
)
