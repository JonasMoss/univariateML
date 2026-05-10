"""Log-Normal distribution — symbolic spec.

Density (in natural params (meanlog, sdlog), sdlog > 0, x > 0):
    log f = -log(x) - log(sdlog) - 0.5 log(2π) - (log(x) - meanlog)² / (2 sdlog²)

Hessian w.r.t. (meanlog, sdlog) is exactly the Normal Hessian on log(x);
the change-of-variable Jacobian sum log(x) is constant in the parameters
and drops out of every derivative.
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
meanlog = sp.Symbol("meanlog", real=True)
sdlog = sp.Symbol("sdlog", positive=True)

log_x = sp.log(x_sym)
half = sp.Rational(1, 2)

log_density_expr = (
    -log_x
    - sp.log(sdlog)
    - half * sp.log(2 * sp.pi)
    - (log_x - meanlog) ** 2 / (2 * sdlog**2)
)

SPEC = Spec(
    id="lnorm",
    params=(
        Param(name="meanlog", start=0.0, domain="real"),
        Param(name="sdlog", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    references={"r": "stats::dlnorm(x, meanlog, sdlog)"},
)
