"""Beta-Prime distribution — symbolic spec.

Density (in natural params (shape1=alpha, shape2=beta), both > 0, x > 0):
    log f = (alpha - 1)*log(x) - (alpha + beta)*log(1 + x)
          - lgamma(alpha) - lgamma(beta) + lgamma(alpha + beta)

Closed-form Fisher info per obs (data-independent — same shape as Beta on
the transformed variable):
    I_aa = trigamma(alpha) - trigamma(alpha + beta)
    I_bb = trigamma(beta)  - trigamma(alpha + beta)
    I_ab = -trigamma(alpha + beta)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
alpha = sp.Symbol("shape1", positive=True)
beta = sp.Symbol("shape2", positive=True)

log_density_expr = (
    (alpha - 1) * sp.log(x_sym)
    - (alpha + beta) * sp.log(1 + x_sym)
    - sp.loggamma(alpha)
    - sp.loggamma(beta)
    + sp.loggamma(alpha + beta)
)

trigamma_ab = sp.polygamma(1, alpha + beta)

SPEC = Spec(
    id="betapr",
    params=(
        Param(name="shape1", start=2.0, domain="(0, oo)"),
        Param(name="shape2", start=2.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    fisher_info_per_obs=sp.Matrix(
        [
            [sp.polygamma(1, alpha) - trigamma_ab, -trigamma_ab],
            [-trigamma_ab, sp.polygamma(1, beta) - trigamma_ab],
        ]
    ),
    references={"r": "extraDistr::dbetapr(x, shape1, shape2)"},
)
