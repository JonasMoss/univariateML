"""Discrete Uniform distribution — symbolic spec.

PMF (in natural params (min=a, max=b), a ≤ b, x ∈ {a, a+1, ..., b}):
    p(x; a, b) = 1 / (b − a + 1)
    log p = −log(b − a + 1)

The C++ Cost treats (a, b) as real-valued for the smooth-Hessian
calculation, so the spec mirrors that convention. Both params are
boundary (MLE a = min(x), MLE b = max(x)).
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
a = sp.Symbol("min", real=True)
b = sp.Symbol("max", real=True)

log_density_expr = -sp.log(b - a + 1)

SPEC = Spec(
    id="dunif",
    params=(
        Param(name="min", start=0.0, domain="real", boundary="upper"),
        Param(name="max", start=10.0, domain="real", boundary="lower"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="{min, ..., max}",
    references={"r": "extraDistr::ddunif(x, min, max)"},
)
