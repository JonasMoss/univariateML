"""Normal distribution — symbolic spec.

Density (in natural params (mean, sd), sd > 0):
    f(x) = (1/(sd √(2π))) exp(−(x − mean)² / (2 sd²))

Cost::operator() takes natural params directly (no phi reparam) and
implements operator(), gradient(), hessian() all analytically. This makes
norm the multi-D analytical-everything pilot — gap between the 1-D exp
case and the multi-D FD-grad std case.

Pure libm — no Boost.Math special functions. The original "Boost.Math
seam" pilot framing in the plan was wrong (gamma/beta are the right
candidates for that; deferred follow-on).
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
mean = sp.Symbol("mean", real=True)
sd = sp.Symbol("sd", positive=True)
p = sp.Symbol("p", positive=True)

half = sp.Rational(1, 2)

log_density_expr = (
    -half * sp.log(2 * sp.pi)
    - sp.log(sd)
    - (x_sym - mean) ** 2 / (2 * sd**2)
)

SPEC = Spec(
    id="norm",
    params=(
        Param(name="mean", start=0.0, domain="real"),
        Param(name="sd", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    cdf=half * (1 + sp.erf((x_sym - mean) / (sd * sp.sqrt(2)))),
    quantile=mean + sd * sp.sqrt(2) * sp.erfinv(2 * p - 1),
    fisher_info_per_obs=sp.Matrix([[1 / sd**2, 0], [0, 2 / sd**2]]),
    references={"r": "stats::dnorm(x, mean, sd)"},
    notes="Closed-form MLE: mean = mean(x), sd = sqrt(mean((x - mean)^2)).",
)
