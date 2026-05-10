"""Geometric distribution — symbolic spec.

PMF (R convention, x ∈ {0, 1, 2, ...}, prob ∈ (0, 1)):
    f(x; p) = p · (1 − p)^x
    log f = log(p) + x · log(1 − p)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", nonnegative=True, integer=True)
p = sp.Symbol("prob", positive=True)

log_density_expr = sp.log(p) + x_sym * sp.log(1 - p)

SPEC = Spec(
    id="geom",
    params=(Param(name="prob", start=0.5, domain="(0, 1)"),),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="{0, 1, 2, ...}",
    fisher_info_per_obs=sp.Matrix([[1 / (p**2 * (1 - p))]]),
    references={"r": "stats::dgeom(x, prob)"},
)
