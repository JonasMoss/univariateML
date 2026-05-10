"""Weibull distribution — symbolic spec.

Density (in natural params (shape s, scale c), s > 0, c > 0, x > 0):
    log f = log(s) - log(c) + (s-1)*(log(x) - log(c)) - (x/c)^s
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
s = sp.Symbol("shape", positive=True)
c = sp.Symbol("scale", positive=True)

log_density_expr = (
    sp.log(s) - sp.log(c) + (s - 1) * (sp.log(x_sym) - sp.log(c)) - (x_sym / c) ** s
)

SPEC = Spec(
    id="weibull",
    params=(
        Param(name="shape", start=1.0, domain="(0, oo)"),
        Param(name="scale", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    cdf=1 - sp.exp(-((x_sym / c) ** s)),
    references={"r": "stats::dweibull(x, shape, scale)"},
)
