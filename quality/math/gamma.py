"""Gamma distribution — symbolic spec.

Density (in natural params (shape=a, rate=r), a > 0, r > 0):
    f(x) = r^a / Γ(a) · x^(a-1) · exp(-r·x),  x > 0

Cost::operator() takes natural params directly. Cost::gradient and
Cost::hessian use Boost.Math digamma / trigamma at MLE-side; this is
the first pilot exercising the Boost.Math seam, which means the
harness's mpmath-50 evaluation of sp.polygamma is being compared
against Boost's digamma/trigamma at C++ double precision.

Closed-form Fisher info per obs (data-independent at the MLE):
    I(a, r) = [[trigamma(a),  -1/r],
               [-1/r,           a/r²]]
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
a = sp.Symbol("shape", positive=True)
r = sp.Symbol("rate", positive=True)
p = sp.Symbol("p", positive=True)

log_density_expr = (
    a * sp.log(r)
    - sp.loggamma(a)
    + (a - 1) * sp.log(x_sym)
    - r * x_sym
)

SPEC = Spec(
    id="gamma",
    params=(
        Param(name="shape", start=2.0, domain="(0, oo)"),
        Param(name="rate", start=2.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    cdf=None,  # lower-incomplete-gamma; sympy can express it but base R doesn't expose it cheaply
    quantile=None,
    fisher_info_per_obs=sp.Matrix(
        [[sp.polygamma(1, a), -1 / r], [-1 / r, a / r**2]]
    ),
    references={"r": "stats::dgamma(x, shape, rate)"},
    notes=(
        "Newton-Raphson on shape via the digamma equation; "
        "rate = shape / mean(x) once shape converges."
    ),
)
