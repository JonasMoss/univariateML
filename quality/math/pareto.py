"""Pareto distribution — symbolic spec.

Density (in natural params (shape=a, min=b), a > 0, b > 0, x ≥ b):
    f(x; a, b) = a · b^a / x^(a+1)
    log f = log(a) + a · log(b) − (a+1) · log(x)

`b` is the lower-support boundary; MLE b̂ = min(x) sits AT the
feasibility limit — non-regular, not asymptotically normal.
The harness sweeps b in the smooth interior (b < min(x)) via
Param.boundary="upper".
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
a = sp.Symbol("a", positive=True)
b = sp.Symbol("b", positive=True)

log_density_expr = sp.log(a) + a * sp.log(b) - (a + 1) * sp.log(x_sym)

SPEC = Spec(
    id="pareto",
    params=(
        Param(name="a", start=1.0, domain="(0, oo)"),
        Param(name="b", start=1.0, domain="(0, oo)", boundary="upper"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="[b, oo)",
    cdf=1 - (b / x_sym) ** a,
    references={"r": "extraDistr::dpareto(x, a, b)"},
)
