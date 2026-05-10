"""Beta distribution — symbolic spec.

Density (in natural params (shape1=a, shape2=b), both > 0, x in (0, 1)):
    log f = (a-1)*log(x) + (b-1)*log(1-x) - lgamma(a) - lgamma(b) + lgamma(a+b)

Closed-form Fisher info per obs (data-independent):
    I_aa = trigamma(a) - trigamma(a+b)
    I_bb = trigamma(b) - trigamma(a+b)
    I_ab = -trigamma(a+b)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
a = sp.Symbol("shape1", positive=True)
b = sp.Symbol("shape2", positive=True)

log_density_expr = (
    (a - 1) * sp.log(x_sym)
    + (b - 1) * sp.log(1 - x_sym)
    - sp.loggamma(a)
    - sp.loggamma(b)
    + sp.loggamma(a + b)
)

trigamma_ab = sp.polygamma(1, a + b)

SPEC = Spec(
    id="beta",
    params=(
        Param(name="shape1", start=2.0, domain="(0, oo)"),
        Param(name="shape2", start=2.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, 1)",
    fisher_info_per_obs=sp.Matrix(
        [
            [sp.polygamma(1, a) - trigamma_ab, -trigamma_ab],
            [-trigamma_ab, sp.polygamma(1, b) - trigamma_ab],
        ]
    ),
    references={"r": "stats::dbeta(x, shape1, shape2)"},
)
