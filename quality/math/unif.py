"""Continuous Uniform distribution — symbolic spec.

Density (in natural params (min=a, max=b), a < b, x ∈ [a, b]):
    f(x; a, b) = 1 / (b − a)
    log f = −log(b − a)

Both parameters sit at feasibility boundaries: MLE a = min(x),
MLE b = max(x). The C++ Cost's gradient/Hessian formulas are valid
in the smooth interior (a < min(x) < max(x) < b); the harness sweeps
inside the smooth region via Param.boundary.
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
a = sp.Symbol("min", real=True)
b = sp.Symbol("max", real=True)

log_density_expr = -sp.log(b - a)

SPEC = Spec(
    id="unif",
    params=(
        Param(name="min", start=0.0, domain="real", boundary="upper"),
        Param(name="max", start=1.0, domain="real", boundary="lower"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(min, max)",
    references={"r": "stats::dunif(x, min, max)"},
)
