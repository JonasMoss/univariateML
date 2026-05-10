"""Poisson distribution — symbolic spec.

PMF (in natural param λ > 0, x ∈ {0, 1, 2, ...}):
    f(x; λ) = λ^x · exp(-λ) / x!

`PoisCost::operator()` drops the `loggamma(x + 1)` constant per its
"+ const" convention; we match here so nll values agree at 1e-12 (the
constant cancels in derivatives, so grad / Hessian aren't affected).
    log f (up to const) = x · log(λ) − λ
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", nonnegative=True, integer=True)
lam = sp.Symbol("lambda", positive=True)

log_density_expr = x_sym * sp.log(lam) - lam

SPEC = Spec(
    id="pois",
    params=(Param(name="lambda", start=1.0, domain="(0, oo)"),),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="{0, 1, 2, ...}",
    fisher_info_per_obs=sp.Matrix([[1 / lam]]),
    references={"r": "stats::dpois(x, lambda)"},
)
