"""Negative-Binomial distribution — symbolic spec.

PMF (in natural params (size = r, prob = p), r > 0, p ∈ (0, 1),
x ∈ {0, 1, 2, ...}):
    log f = loggamma(x + r) − loggamma(r) − loggamma(x + 1)
          + r · log(p) + x · log(1 − p)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", nonnegative=True, integer=True)
r = sp.Symbol("size", positive=True)
p = sp.Symbol("prob", positive=True)

log_density_expr = (
    sp.loggamma(x_sym + r)
    - sp.loggamma(r)
    - sp.loggamma(x_sym + 1)
    + r * sp.log(p)
    + x_sym * sp.log(1 - p)
)

SPEC = Spec(
    id="nbinom",
    params=(
        Param(name="size", start=2.0, domain="(0, oo)"),
        Param(name="prob", start=0.5, domain="(0, 1)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="{0, 1, 2, ...}",
    references={"r": "stats::dnbinom(x, size, prob)"},
)
