"""Log-Logistic distribution — symbolic spec.

Density (in natural params (shape a, rate r), a > 0, r > 0, x > 0):
    log f = log(a) + log(r) + (a-1)*(log(r) + log(x)) - 2*log(1 + (rx)^a)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
a = sp.Symbol("shape", positive=True)
r = sp.Symbol("rate", positive=True)

rx = r * x_sym
log_density_expr = (
    sp.log(a)
    + sp.log(r)
    + (a - 1) * (sp.log(r) + sp.log(x_sym))
    - 2 * sp.log(1 + rx**a)
)

SPEC = Spec(
    id="llogis",
    params=(
        Param(name="shape", start=1.0, domain="(0, oo)"),
        Param(name="rate", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    cdf=1 - 1 / (1 + rx**a),
    references={"r": "actuar::dllogis(x, shape, rate)"},
)
