"""Inverse Burr distribution — symbolic spec.

Density (in natural params (shape1=k, shape2=c) with scale=1, both > 0, x > 0):
    log f = log(k) + log(c) + (k·c - 1)*log(x) - (k+1)*log(1 + x^c)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
k = sp.Symbol("shape1", positive=True)
c = sp.Symbol("shape2", positive=True)

log_density_expr = (
    sp.log(k)
    + sp.log(c)
    + (k * c - 1) * sp.log(x_sym)
    - (k + 1) * sp.log(1 + x_sym**c)
)

SPEC = Spec(
    id="invburr",
    params=(
        Param(name="shape1", start=1.0, domain="(0, oo)"),
        Param(name="shape2", start=2.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    references={"r": "actuar::dinvburr(x, shape1, shape2)"},
)
