"""Paralogistic distribution — symbolic spec.

Density in natural params (shape α, scale θ), both > 0, x > 0:
    f(x; α, θ) = α² · (x/θ)^α / [x · (1 + (x/θ)^α)^(α+1)]

Cost::operator() takes phi = (log α, log θ); BFGS optimises with
finite_diff_cost wrapper (no analytical gradient). The symbolic spec
mirrors that — log_density is written in (u = log α, v = log θ) so
sympy derivatives compare directly against `Cost::hessian`.
"""

import math

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
u = sp.Symbol("log_shape", real=True)
v = sp.Symbol("log_scale", real=True)

alpha = sp.exp(u)
log_z = sp.log(x_sym) - v  # log(x / theta)
t = alpha * log_z

log_density_expr = (
    2 * u
    + alpha * log_z
    - sp.log(x_sym)
    - (alpha + 1) * sp.log(1 + sp.exp(t))
)

SPEC = Spec(
    id="paralogis",
    params=(
        Param(name="log_shape", start=0.0, domain="real"),
        Param(name="log_scale", start=0.0, domain="real"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    references={"r": "actuar::dparalogis(x, shape, scale)"},
    cost_has_gradient=False,
    golden_to_cost=lambda e: [
        math.log(float(e["shape"])),
        math.log(float(e["scale"])),
    ],
)
