"""Power distribution — symbolic spec.

Density (in natural params (alpha=upper-bound, beta=shape),
alpha > 0, beta > 0, 0 < x < alpha):
    f(x; α, β) = β · x^(β−1) / α^β
    log f = log(β) + (β−1) · log(x) − β · log(α)

`alpha` is the upper-support boundary; MLE α̂ = max(x) sits AT the
feasibility limit — non-regular. The harness sweeps alpha in the
smooth interior (alpha > max(x)) via Param.boundary="lower".
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
alpha = sp.Symbol("alpha", positive=True)
beta = sp.Symbol("beta", positive=True)

log_density_expr = (
    sp.log(beta) + (beta - 1) * sp.log(x_sym) - beta * sp.log(alpha)
)

SPEC = Spec(
    id="power",
    params=(
        Param(name="alpha", start=1.0, domain="(0, oo)", boundary="lower"),
        Param(name="beta", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, alpha)",
    cdf=(x_sym / alpha) ** beta,
    references={"r": "extraDistr::dpower(x, alpha, beta)"},
)
