"""Logarithmic Series distribution — symbolic spec.

PMF (in natural param theta ∈ (0, 1), x ∈ {1, 2, 3, ...}):
    f(x; θ) = -θ^x / (x · log(1 − θ))

`LgserCost::operator()` drops the data-only `log(x)` constant per its
"+ const" convention; we match here so nll values agree at 1e-12 (the
constant cancels in derivatives, so grad / Hessian aren't affected).
    log f (up to const) = x · log(θ) − log(−log(1 − θ))
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True, integer=True)
theta = sp.Symbol("theta", positive=True)

log_density_expr = (
    x_sym * sp.log(theta) - sp.log(-sp.log(1 - theta))
)

SPEC = Spec(
    id="lgser",
    params=(Param(name="theta", start=0.5, domain="(0, 1)"),),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="{1, 2, 3, ...}",
    references={"r": "extraDistr::dlgser(x, theta)"},
)
